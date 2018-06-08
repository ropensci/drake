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
  on.exit(mc_set_done_all(config))
  config$queue <- new_target_queue(config = config)
  while (TRUE){
    assignment_queues <- mc_message_queues(config, "assignments")
    completed_queues <- mc_message_queues(config, "completed")
    if (!mc_work_remains(assignment_queues, config)){
      return()
    }
    mc_clear_completed_queues(completed_queues, config)
    mc_assign_targets(assignment_queues, config)
    Sys.sleep(mc_wait)
  }
}

mc_assign_targets <- function(assignemnt_queues, config){
  if (!length(assignment_queues)){
    return()
  }
  while(length(target <- config$queue$peek0()) > 0){
    
    
  }
}

mc_message_queues <- function(config, type){
  lapply(
    mc_list_dbs(config),
    function(db){
      ensure_queue(type, db = db)
    }
  )
}

mc_worker <- function(worker, config){
  on.exit(mc_delete_queue(meta, force = TRUE))
  db <- file.path(config$scratch_dir, worker, ".db")
  assignments <- mc_ensure_queue("assignments", db = db)
  completed <- mc_ensure_queue("completed", db = db)
  meta <- mc_ensure_queue("meta", db = db)
  mc_publish(meta, "status", "active")
  while (TRUE){
    if (!file.exists(db)){
      return()
    }
    while (is.null(msg <- mc_try_consume(assignments))){
      Sys.sleep(mc_wait)
    }
    if (identical(msg$title, "done")){
      return()
    }
    target <- msg$message
    build_check_store(
      target = target,
      config = config,
      downstream = config$cache$list(namespace = "mc_protect")
    )
    mc_publish(queue = completed, title = "target", message = target)
  }
}

mc_init_worker_cache <- function(config){
  for (namespace in c("mc_protect")){
    config$cache$clear(namespace = namespace)
  }
  fs::dir_create(file.path(config$scratch_dir))
  lapply(
    X = igraph::V(config$schedule)$name,
    FUN = function(target){
      config$cache$set(key = target, value = TRUE, namespace = "mc_protect")
    }
  )
  invisible()
}

mc_clear_completed_queues <- function(completed_queues, config){
  targets <- character(0)
  for(queue in completed_queues){
    while (!is.null(msg <- mc_try_consume(queue))){
      if (identical(msg$title, "target")){
        mc_conclude_target(target = msg$message, config = config)
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
  if (
    identical(
      get_progress_single(target = target, cache = config$cache),
      "finished"
    ) && target %in% config$plan$target
  ){
    set_attempt_flag(key = worker, config = config)
  }
}

mc_list_dbs <- function(config){
  list.files(config$scratch_dir) %>%
    grep(".db$", fixed = TRUE, value = TRUE) 
}

mc_work_remains <- function(assignment_queues, config){
  empty <- config$queue$empty()
  n_pending_targets <- vapply(
    X = assignment_queues,
    FUN = function(queue){
      nrow(mc_list_messages(queue))
    },
    FUN.VALUE = integer(1)
  )
  all_done <- all(n_pending_targets < 1)
  if (!empty && all_done){
    stop("workers finished without completing their work.") # nocov
  }
  !empty || !all_done
}

mc_set_done_all <- function(config){
  lapply(mc_list_dbs(config), function(db){
    meta <- mc_ensure_queue("meta", db = db)
    mc_delete_queue(meta, force = TRUE)
  })
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

mc_should_assign_target <- function(worker, target, config){
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
