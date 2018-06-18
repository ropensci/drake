mc_init_worker_cache <- function(config){
  namespaces <- c("mc_protect", "mc_ready_db", "mc_done_db")
  for (namespace in namespaces){
    config$cache$clear(namespace = namespace)
  }
  dir_empty(worker_dir <- file.path(config$cache_path, "workers"))
  lapply(
    X = seq_len(config$jobs),
    FUN = function(worker){
      worker <- mc_worker_id(worker)
      for (type in c("ready", "done")){
        namespace <- paste0("mc_", type, "_db")
        db <- paste0(worker, "_", type)
        config$cache$set(
          key = worker,
          value = file.path(worker_dir, db),
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

mc_ensure_workers <- function(config){
  paths <- vapply(
    X = config$cache$list(namespace = "mc_ready_db"),
    FUN = function(worker){
      config$cache$get(key = worker, namespace = "mc_ready_db")
    },
    FUN.VALUE = character(1)
  )
  while (!all(file.exists(paths))){
    Sys.sleep(mc_wait) # nocov
  }
}

mc_work_remains <- function(config){
  if (!config$queue$empty()){
    return(TRUE)
  }
  backlog <- vapply(
    config$mc_ready_queues,
    function(queue){
      queue$count()
    },
    FUN.VALUE = integer(1)
  )
  any(backlog > 0)
}

mc_refresh_queue_lists <- function(config){
  for (namespace in c("mc_ready_db", "mc_done_db")){
    field <- gsub("db$", "queues", namespace)
    unlogged_workers <- setdiff(
      config$cache$list(namespace), names(config[[field]]))
    for (worker in unlogged_workers){
      path <- config$cache$get(key = worker, namespace = namespace)
      if (file.exists(path)){
        config[[field]][[worker]] <- txtq::txtq(path)
      }
    }
  }
  config
}

mc_assign_ready_targets <- function(config){
  if (!length(config$mc_ready_queues)){
    return()
  }
  for (target in config$queue$list0()){
    queue <- mc_preferred_queue(target = target, config = config)
    if (!is.null(queue)){
      queue$push(title = target, message = "target")
      config$queue$remove(targets = target)
    }
  }
}

mc_preferred_queue <- function(target, config){
  if ("worker" %in% colnames(config$plan) && target %in% config$plan$target){
    worker <- mc_worker_id(
      config$plan[["worker"]][config$plan$target == target]
    )
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
  for (queue in config$mc_ready_queues){
    if (queue$empty()){
      return(queue)
    }
  }
  return(NULL)
}

mc_conclude_done_targets <- function(config, wait_for_checksums = TRUE){
  for (queue in config$mc_done_queues){
    while (nrow(msg <- queue$pop(1)) > 0){
      if (wait_for_checksums){
        mc_wait_checksum(
          target = msg$title, checksum = msg$message, config = config)
      }
      mc_conclude_target(target = msg$title, config = config)
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

mc_conclude_workers <- function(config){
  lapply(
    X = config$cache$list("mc_ready_db"),
    FUN = function(worker){
      queue <- mc_get_ready_queue(worker, config)
      queue$push(title = "done", message = "done")
    }
  )
}

mc_get_worker_queue <- function(worker, namespace, config){
  while (!config$cache$exists(key = worker, namespace = namespace)){
    Sys.sleep(mc_wait) # nocov
  }
  txtq::txtq(config$cache$get(key = worker, namespace = namespace))
}

mc_get_ready_queue <- function(worker, config){
  mc_get_worker_queue(
    worker = worker, namespace = "mc_ready_db", config = config
  )
}

mc_get_done_queue <- function(worker, config){
  mc_get_worker_queue(
    worker = worker, namespace = "mc_done_db", config = config
  )
}

mc_worker_id <- function(x){
  paste0("worker_", x)
}

mc_get_checksum <- function(target, config){
  paste(
    safe_get_hash(
      key = target,
      namespace = config$cache$default_namespace,
      config = config
    ),
    safe_get_hash(key = target, namespace = "kernels", config = config),
    safe_get_hash(key = target, namespace = "meta", config = config),
    safe_get_hash(key = target, namespace = "attempt", config = config),
    sep = " "
  )
}

mc_is_good_checksum <- function(target, checksum, config){
  stamp <- mc_get_checksum(target = target, config = config)
  if (!identical(stamp, checksum)){
    return(FALSE)
  }
  if (identical("failed", get_progress_single(target, cache = config$cache))){
    return(TRUE) # covered with parallel processes # nocov
  }
  all(
    vapply(
      X = unlist(strsplit(stamp, " "))[1:3], # Exclude attempt flag (often NA).
      config$cache$exists_object,
      FUN.VALUE = logical(1)
    )
  )
}

mc_wait_checksum <- function(target, checksum, config, timeout = 300){
  interval <- 0.01 # Should be longer than mc_wait.
  i <- 0
  while (i < timeout / interval){
    if (mc_is_good_checksum(target, checksum, config)){
      return()
    } else {
      Sys.sleep(interval)
    }
    i <- i + 1
  }
  drake_error(
    "Target `", target, "` did not download from your ",
    "network file system. Checksum verification timed out after about ",
    timeout, " seconds.", config = config
  )
}

mc_wait <- 1e-9
