run_future <- function(config){
  queue <- new_target_queue(config = config)
  workers <- initialize_workers(config)
  # While any targets are queued or running...
  while (work_remains(queue = queue, workers = workers, config = config)){
    for (id in seq_along(workers)){
      if (is_idle(workers[[id]])){
        # Should also decrease key in the priority queue
        # so all the available targets are at the head.
        workers[[id]] <- conclude_worker(
          worker = workers[[id]],
          config = config
        )
        if (!queue$size()){
          next
        }

        # With a priority queue, we should not need to 
        # check dependencies in a manner this slow.
        next_target <- queue$peek(n = 1)
        next_target_deps <- dependencies(
          targets = next_target, config = config)
        running <- running_targets(workers = workers, config = config)
        running_deps <- intersect(running, next_target_deps)
        if (length(running_deps)){
          next
        }
        queue$pop(n = 1)

        protect <- c(running, queue$list())
        workers[[id]] <- active_worker(
          id = id,
          target = next_target,
          config = config,
          protect = protect
        )
      }
    }
    Sys.sleep(1e-9)
  }
}

#' @title Task passed to individual futures in the `"future"` backend
#' @description For internal use only. Only exported to make available
#' to futures.
#' @keywords internal
#' @export
#' @return Either the target value or a list of build results.
#' @param target name of the target
#' @param meta list of metadata
#' @param config [drake_config()] list
#' @param protect Names of targets that still need their
#' dependencies available in `config$envir`.
drake_future_task <- function(target, meta, config, protect){
  prune_envir(
    targets = target, config = config, downstream = protect)
  do_prework(config = config, verbose_packages = FALSE)
  if (config$caching == "worker"){
    build_and_store(target = target, meta = meta, config = config)
  } else {
    announce_build(target = target, meta = meta, config = config)
    config$hook(just_build(target = target, meta = meta, config = config))
  }
}

active_worker <- function(id, target, config, protect){
  meta <- drake_meta(target = target, config = config)
  if (!should_build_target(
    target = target,
    meta = meta,
    config = config
  )){
    return(empty_worker(target = target))
  }
  structure(
    future::future(
      expr = drake_future_task(
        target = target, meta = meta, config = config, protect = protect),
      packages = "drake"
    ),
    target = target
  )
}

empty_worker <- function(target){
  structure(NA, target = target)
}

is_empty_worker <- function(worker){
  !inherits(worker, "Future")
}

concluded_worker <- function(){
  empty_worker(target = NULL)
}

is_concluded_worker <- function(worker){
  is.null(attr(worker, "target"))
}

# Need to check if the worker quit in error early somehow.
# Maybe the job scheduler failed.
# This should be the responsibility of the `future` package
# or something lower level.
is_idle <- function(worker){
  is_empty_worker(worker) ||
    is_concluded_worker(worker) ||
    future::resolved(worker)
}

work_remains <- function(queue, workers, config){
  !queue$empty() ||
    !all_concluded(workers = workers, config = config)
}

all_concluded <- function(workers, config){
  lightly_parallelize(
    X = workers,
    FUN = function(worker){
      is_concluded_worker(worker)
    },
    jobs = config$jobs
  ) %>%
    unlist %>%
    all
}

running_targets <- function(workers, config){
  lightly_parallelize(
    X = workers,
    FUN = function(worker){
      if (is_idle(worker)){
        NULL
      } else {
        attr(worker, "target")
      }
    },
    jobs = config$jobs
  ) %>%
    unlist
}

initialize_workers <- function(config){
  config$cache$clear(namespace = "workers")
  as.list(rep(empty_worker(target = NA), length(config$jobs)))
}

# Currently only needed for "future_commands" workers
# since "future_total" workers already conclude the build
# and store the results.
conclude_worker <- function(target, worker, config){
  out <- concluded_worker()
  if (is_empty_worker(worker)){
    return(out)
  }
  set_attempt_flag(config = config)
  # Here, we should also check if the future resolved due to an error.
  if (config$caching == "worker"){
    return(out)
  }
  build <- future::value(worker)
  config$hook({
    conclude_build(
      target = build$target,
      value = build$value,
      meta = build$meta,
      config = config
    )
  })
  out
}
