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

mc_refresh_queue_lists <- function(config){
  for (namespace in c("mc_ready_db", "mc_done_db")){
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
        queue$path
      },
      FUN.VALUE = character(1)
    )
    names(possible_dbs) <- config$cache$list(namespace)
    created_dbs <- possible_dbs[file.exists(possible_dbs)]
    new_dbs <- created_dbs[!(created_dbs %in% old_dbs)] # keeps names
    new_queues <- lapply(new_dbs, message_queue, create = FALSE)
    config[[field]] <- c(config[[field]], new_queues)
  }
  config
}

mc_assign_ready_targets <- function(config){
  if (!length(config$mc_ready_queues)){
    return()
  }
  while (!is.null(target <- config$queue$peek0())){
    queue <- mc_preferred_queue(target = target, config = config)
    queue$push(title = target, message = "target")
    config$queue$pop0()
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
  backlog <- vapply(
    config$mc_ready_queues,
    function(queue){
      queue$count()
    },
    FUN.VALUE = integer(1)
  )
  config$mc_ready_queues[[which.min(backlog)]]
}

mc_conclude_done_targets <- function(config){
  for (queue in config$mc_done_queues){
    while (nrow(messages <- queue$pop(-1)) > 0){
      for (i in seq_len(nrow(messages))){
        msg <- messages[i, ]
        if (identical(msg$message, "target")){
          mc_conclude_target(target = msg$title, config = config)
        } else {
          drake_error("illegal message type in the done queue", config = config) # nocov # nolint
        }
      }
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
  config$cache$get(key = worker, namespace = namespace) %>%
    message_queue(create = TRUE)
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

mc_wait <- 1e-9
