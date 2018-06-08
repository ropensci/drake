####################################
### HIGH-LEVEL PROCESS FUNCTIONS ###
####################################

run_mclapply <- function(config){
  if (config$jobs < 2 && !length(config$debug)) {
    return(run_loop(config = config))
  }
  mc_init_worker_cache(config)
  tmp <- mclapply(
    X = c(0, seq_len(config$jobs)),
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
      if (id < 1){
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
    assignment_queues <- mc_message_queues(config, "assignments")
    if (!mc_work_remains(assignment_queues, config)){
      return()
    }
    mc_clear_completed_targets(config)
    mc_assign_targets(assignment_queues, config)
    Sys.sleep(mc_wait)
  }
}

mc_worker <- function(worker, config){
  db <- mc_db_file(worker = worker, config = config)
  assignments <- mc_ensure_queue("assignments", db = db)
  completed <- mc_ensure_queue("completed", db = db)
  while (TRUE){
    while (is.null(msg <- mc_try_consume(assignments))){
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
    mc_publish(queue = completed, title = target, message = "target")
  }
}

##################################
### MID-LEVEL SUPPORTING IDEAS ###
##################################

mc_init_worker_cache <- function(config){
  for (namespace in c("mc_protect")){
    config$cache$clear(namespace = namespace)
  }
  fs::dir_create(file.path(config$scratch_dir))
  dir_empty(config$scratch_dir)
  lapply(
    X = igraph::V(config$schedule)$name,
    FUN = function(target){
      config$cache$set(key = target, value = TRUE, namespace = "mc_protect")
    }
  )
  invisible()
}

mc_work_remains <- function(assignment_queues, config){
  empty <- config$queue$empty()
  n_worker_messages <- vapply(
    X = assignment_queues,
    FUN = mc_count_messages,
    FUN.VALUE = integer(1)
  )
  !empty || n_worker_messages > 0
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

mc_preferred_queue <- function(assignment_queues, target, config){
  if ("workers" %in% colnames(config$plan) && target %in% config$plan$target){
    db <- mc_db_file(
      worker = config$plan$workers[config$plan$target == target],
      config = config
    )
    dbs <- vapply(
      assignment_queues,
      function(queue){
        queue$db
      },
      FUN.VALUE = character(1)
    )
    if (db %in% dbs){
      return(assignment_queues[db == dbs][[1]])
    } else {
      drake_warning(
        "Preferred worker for target `",
        target,
        "` does not exist (yet).",
        config = config
      )
    }
  }
  backlog <- vapply(
    assignment_queues,
    mc_count_messages,
    FUN.VALUE = integer(1)
  )
  assignment_queues[[which.min(backlog)]]
}

mc_can_assign_target <- function(worker, target, config){
  if (!length(target)){
    return(FALSE)
  }
  if (
    !("workers" %in% colnames(config$plan)) ||
    !(target %in% config$plan$target)
  ){
    return(TRUE)
  }
  allowed_workers <- as.integer(
    unlist(
      config$plan$workers[config$plan$target == target]
    )
  )
  allowed_workers <- allowed_workers %% config$jobs
  allowed_workers[allowed_workers == 0] <- config$jobs
  if (!length(allowed_workers)){
    return(TRUE)
  }
  as.integer(worker) %in% allowed_workers
}

mc_clear_completed_targets <- function(config){
  completed_queues <- mc_message_queues(config, "completed")
  targets <- character(0)
  for(queue in completed_queues){
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
  fs::path_file(fs::path_ext_remove(x$db))
}

mc_message_count <- function(queue){
  nrow(mc_list_messages(queue))
}

mc_message_queues <- function(config, type){
  lapply(
    mc_list_dbs(config),
    function(db){
      mc_ensure_queue(type, db = db)
    }
  )
}

mc_db_file <- function(worker, config){
  file.path(config$scratch_dir, paste0(worker, ".db"))
}

mc_list_dbs <- function(config){
  list.files(config$scratch_dir, full.names = TRUE) %>%
    grep(pattern = ".db$", value = TRUE) 
}

mc_conclude_workers <- function(config){
  lapply(
    X = config$workers,
    FUN = function(worker){
      queue <- mc_ensure_queue(
        "assignments", db = mc_db_file(worker, config)
      )
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
