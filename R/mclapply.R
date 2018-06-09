####################################
### HIGH-LEVEL PROCESS FUNCTIONS ###
####################################

run_mclapply <- function(config){
  if (config$jobs < 2 && !length(config$debug)) {
    return(run_loop(config = config))
  }
  mc_init_worker_cache(config)
  tmp <- mclapply(
    X = mc_worker_id(c(0, seq_len(config$jobs))),
    FUN = mc_process,
    mc.cores = config$jobs + 1,
    config = config
  )
  invisible()
}

#' @title Internal function to launch
#' a master process or persistent worker.
#' @description For internal use only.
#' Exported only for the purpose of
#' using persistent workers in
#' `make(paralellism = "parLapply", jobs = n)`,
#' where `n > 1`.
#' @keywords internal
#' @export
#' @param id character scalar with the job id
#' @param config `drake_config()` list
#' @return nothing important
mc_process <- function(id, config){
  withCallingHandlers(
    expr = {
      if (identical(id, mc_worker_id(0))){
        mc_master(config)
      } else {
        mc_worker(worker = id, config = config)
      }
    },
    error = function(e){
      error_process(e = e, id = id, config = config) # nocov
    },
    warning = function(e){
      warning_process(e = e, id = id, config = config) # nocov
    }
  )
  invisible()
}

#' @title Internal function to launch
#' a master process.
#' @description For internal use only.
#' Exported only for the purpose of
#' using persistent workers in
#' `make(paralellism = "future_lapply", jobs = n)`,
#' where `n > 1`.
#' @keywords internal
#' @export
#' @param config `drake_config()` list
#' @return nothing important
mc_master <- function(config){
  on.exit(mc_conclude_workers(config))
  config$queue <- new_target_queue(config = config)
  while (TRUE){
    config <- refresh_queue_lists(config)
    if (mc_master_done(config)){
      return()
    }
    mc_clear_done_targets(config)
    mc_assign_targets(config)
    Sys.sleep(mc_wait)
  }
}

mc_refresh_queue_lists <- function(config){
  for(namespace in c("mc_ready_db", "mc_done_db")){
    possible_dbs <- unlist(
      config$cache$mget(config$cache$list(namespace), namespace)
    )
    if (!length(possible_dbs)){
      next
    }
    field <- gsub("db$", "queues", namespace)
    old_dbs <- vapply(
      X = config[[field]],
      FUN = function(queue){
        queue$db
      },
      FUN.VALUE = character(1)
    )
    names(possible_dbs) <- config$cache$list(namespace)
    created_dbs <- possible_dbs[file.exists(possible_dbs)]
    new_dbs <- created_dbs[!(created_dbs %in% old_dbs)] # keeps names
    new_queues <- lapply(new_dbs, mc_ensure_queue)
    config[[field]] <- c(config[[field]], new_queues)
    still_exists <- vapply(
      X = config[[field]],
      FUN = function(queue){
        file.exists(queue$db)
      },
      FUN.VALUE = logical(1)
    )
    config[[field]] <- config[[field]][still_exists]
  }
  config
}

mc_worker <- function(worker, config){
  on.exit({
    mc_destroy(ready_queue)
    mc_destroy(done_queue)
  })
  ready_queue <- config$cache$get(key = worker, namespace = "mc_ready_db") %>%
    mc_ensure_queue
  done_queue <- config$cache$get(key = worker, namespace = "mc_done_db") %>%
    mc_ensure_queue
  while (TRUE){
    while (is.null(msg <- mc_try_consume(ready_queue))){
      Sys.sleep(mc_wait)
    }
    if (identical(msg$message, "done")){
      return()
    }
    target <- msg$title
    build_check_store(
      target = target,
      config = config,
      downstream = config$cache$list(namespace = "mc_protect"),
      flag_attempt = TRUE
    )
    mc_ack(msg)
    mc_publish(queue = done_queue, title = target, message = "target")
  }
}

##################################
### MID-LEVEL SUPPORTING IDEAS ###
##################################

mc_init_worker_cache <- function(config){
  for (namespace in c("mc_protect", "mc_ready_db", "mc_done_db")){
    config$cache$clear(namespace = namespace)
  }
  lapply(
    X = seq_len(config$jobs),
    FUN = function(worker){
      for (namespace in c("mc_ready_db", "mc_done_db")){
        config$cache$set(
          key = mc_worker_id(worker),
          value = tempfile(),
          namespace = namespace
        )
      }
    }
  )
  lapply(
    X = igraph::V(config$schedule)$name,
    FUN = function(target){
      config$cache$set(key = target, value = TRUE, namespace = "mc_protect")
    }
  )
  invisible()
}

mc_master_done <- function(config){
  config$queue$empty() &&
    length(config$mc_ready_queues) < 1 &&
    length(config$mc_done_queues) < 1
}

mc_assign_targets <- function(assignment_queues, config){
  if (!length(assignment_queues)){
    return()
  }
  while(!is.null(target <- config$queue$peek0())){
    index <- sample.int(length(assignment_queues), 1)
    queue <- mc_preferred_queue(assignment_queues, target, config)
    mc_publish(queue, title = target, message = "target")
    config$queue$pop0()
  }
}

mc_preferred_queue <- function(target, config){
  if ("worker" %in% colnames(config$plan) && target %in% config$plan$target){
    worker <- mc_worker_id(config$plan$worker[config$plan$target == target])
    if (worker %in% names(config$mc_ready_queues)){
      return(config$mc_ready_queues[[worker]])
    } else {
      drake_warning(
        "Preferred worker for target `",
        target, "` (", worker, ") does not exist (yet).",
        config = config
      )
    }
  }
  backlog <- vapply(
    config$mc_ready_queues,
    mc_count_messages,
    FUN.VALUE = integer(1)
  )
  config$mc_ready_queues[[which.min(backlog)]]
}

mc_clear_completed_targets <- function(config){
  for(queue in config$mc_done_queues){
    while (!is.null(msg <- mc_try_consume(queue))){
      if (identical(msg$message, "target")){
        mc_conclude_target(target = msg$title, config = config)
      }
      mc_ack(msg)
    }
  }
}

mc_conclude_target <- function(target, config){
  config$cache$del(key = target, namespace = "mc_protect")
  revdeps <- dependencies(
    targets = target,
    config = config,
    reverse = TRUE
  ) %>%
    intersect(y = config$queue$list())
  config$queue$decrease_key(targets = revdeps)
}

#######################
### LOW-LEVEL UTILS ###
#######################

# x could be a queue or a message
mc_worker_id <- function(x){
  paste0("worker_", x)
}

mc_conclude_workers <- function(config){
  lapply(
    X = config$cache$list("mc_ready_db"),
    FUN = function(worker){
      queue <- config$cache$get(key = worker, namespace = "mc_ready_db") %>%
        mc_ensure_queue
      mc_publish(queue, title = "done", message = "done")
    }
  )
}

mc_wait <- 1e-9

warn_mclapply_windows <- function(
  parallelism,
  jobs,
  os = this_os()
){
  if (
    "mclapply" %in% parallelism &&
    targets_setting(jobs) > 1 &&
    identical(os, "windows")
  ){
    warning(
      "Demoting to one job at a time (no parallel computing). ",
      "The mclapply parallel backend does not support ",
      "multiple jobs on Windows. Windows users should use the ",
      "parLapply backend intead (Windows default), or an other ",
        "Windows-compatible backend.",
      call. = FALSE
    )
  }
}
