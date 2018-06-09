mc_init_worker_cache <- function(config){
  namespaces <- c("mc_protect", "mc_ready_db", "mc_done_db")
  for (namespace in namespaces){
    config$cache$clear(namespace = namespace)
  }
  dir_empty(temp_dir <- file.path(config$cache_path, "mc"))
  lapply(
    X = seq_len(config$jobs),
    FUN = function(worker){
      for (namespace in c("mc_ready_db", "mc_done_db")){
        config$cache$set(
          key = mc_worker_id(worker),
          value = file.path(temp_dir, tempfile()),
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
  }
  config
}

mc_assign_targets <- function(config){
  if (!length(config$mc_ready_queues)){
    return()
  }
  while(!is.null(target <- config$queue$peek0())){
    mc_preferred_queue(target = target, config = config) %>%
      mc_publish(title = target, message = "target")
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

mc_clear_done <- function(config){
  for(queue in config$mc_done_queues){
    while (!is.null(msg <- mc_try_consume(queue))){
      if (identical(msg$message, "target")){
        mc_conclude_target(target = msg$title, config = config)
      } else {
        drake_error("illegal message type in the done queue", config = config) # nocov # nolint
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

mc_conclude_workers <- function(config){
  lapply(
    X = config$cache$list("mc_ready_db"),
    FUN = function(worker){
      mc_get_ready_queue(worker, config) %>%
        mc_publish(title = "done", message = "done")
    }
  )
}

mc_get_worker_queue <- function(worker, namespace, config){
  while (!config$cache$exists(key = worker, namespace = namespace)){
    Sys.sleep(mc_wait)
  }
  config$cache$get(key = worker, namespace = namespace) %>%
    mc_ensure_queue
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

mc_lock <- function(code, db){
  on.exit(filelock::unlock(db_lock))
  db_lock <- filelock::lock(fs::path_ext_set(db, "lock"))
  force(code)
}

mc_ack <- function(msg){
  mc_lock({
    try(DBI::dbDisconnect(msg$lock), silent = TRUE)
    liteq::ack(msg)
  },
  msg$db
  )
}

mc_ensure_queue <- function(db){
  mc_lock(liteq::ensure_queue(name = "jobs", db = db), db)
}

mc_list_messages <- function(queue){
  mc_lock(liteq::list_messages(queue), queue$db)
}

mc_count_messages <- function(queue){
  nrow(mc_list_messages(queue))
}

mc_publish <- function(queue, title, message){
  mc_lock(
    liteq::publish(queue = queue, title = title, message = message),
    queue$db
  )
}

mc_try_consume <- function(queue){
  mc_lock(liteq::try_consume(queue), queue$db)
}

mc_set_done <- function(queue){
  mc_publish()
}

mc_wait <- 1e-9
