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
  config$queue <- new_target_queue(config = config)
  while (TRUE){
    
    browser()
    
    assignment_queues <- mc_message_queues(config, "assignments")
    if (!mc_work_remains(assignment_queues, config)){
      return()
    }
    mc_clear_completed_targets(config)
    mc_assign_targets(assignment_queues, config)
    Sys.sleep(mc_wait)
  }
}

mc_assign_targets <- function(assignment_queues, config){
  if (!length(assignment_queues)){
    return()
  }
  while(length(target <- config$queue$peek0()) > 0){
    worker_ids <- vapply(
      assignment_queues, mc_worker_id, FUN.VALUE = character(1)
    )
    backlog <- vapply(
      assignment_queues, mc_message_count, FUN.VALUE = numeric(1)
    ) %>%
      stats::setNames(nm = worker_ids)
    can_assign <- vapply(
      worker_ids, mc_can_assign_target, FUN.VALUE = logical(1),
      target = target, config = config
    )
    backlog <- backlog[can_assign]
    index <- as.integer(names(which.min(backlog)))
    queue <- assignment_queues[[index]]
    mc_publish(queue, title = target, message = "target")
    config$queue$pop0()
  }
}

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
      liteq::ensure_queue(type, db = db)
    }
  )
}

mc_db_file <- function(worker, config){
  file.path(config$scratch_dir, paste0(worker, ".db"))
}

mc_worker <- function(worker, config){
  db <- mc_db_file(worker = worker, config = config)
  assignments <- liteq::ensure_queue("assignments", db = db)
  completed <- liteq::ensure_queue("completed", db = db)
  while (TRUE){
    
    browser()
    
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
      downstream = config$cache$list(namespace = "mc_protect")
    )
    mc_ack(msg)
    mc_publish(queue = completed, title = target, message = "target")
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

mc_clear_completed_targets <- function(config){
  completed_queues <- mc_message_queues(config, "completed")
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
  list.files(config$scratch_dir, full.names = TRUE) %>%
    grep(pattern = ".db$", value = TRUE) 
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
  !empty || !all_done
}

mc_set_done_all <- function(config){
  lapply(mc_list_dbs(config), unlink, force = TRUE)
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
