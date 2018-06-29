run_future <- function(config){
  queue <- new_priority_queue(config = config)
  workers <- initialize_workers()
  # While any targets are queued or running...
  while (work_remains(queue = queue, workers = workers, config = config)){
    for (id in seq_along(workers)){
      if (is_idle(workers[[id]])){
        # Also calls decrease-key on the queue.
        workers[[id]] <- conclude_worker(
          worker = workers[[id]],
          config = config,
          queue = queue
        )
        # Pop the head target only if its priority is 0
        next_target <- queue$pop0()
        if (!length(next_target)){
          # It's hard to make this line run in a small test workflow
          # suitable enough for unit testing, but
          # I did artificially stall targets and verified that this line
          # is reached in the future::multisession backend as expected.
          next # nocov
        }
        running <- running_targets(workers = workers, config = config)
        protect <- c(running, queue$list())
        workers[[id]] <- new_worker(
          id = id,
          target = next_target,
          config = config,
          protect = protect
        )
      }
    }
    Sys.sleep(mc_wait)
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
drake_future_task <- function(target, meta, config){
  do_prework(config = config, verbose_packages = FALSE)
  if (config$caching == "worker"){
    build_and_store(
      target = target, meta = meta, config = config, announce = FALSE)
  } else {
    just_build(target = target, meta = meta, config = config)
  }
}

new_worker <- function(id, target, config, protect){
  meta <- drake_meta(target = target, config = config)
  if (!should_build_target(
    target = target,
    meta = meta,
    config = config
  )){
    return(empty_worker(target = target))
  }
  prune_envir(targets = target, config = config, downstream = protect)
  meta$start <- proc.time()
  config$cache$flush_cache() # Less data to pass this way.
  DRAKE_GLOBALS__ <- NULL # Fixes warning about undefined globals.
  # Avoid potential name conflicts with other globals.
  # When we solve #296, the need for such a clumsy workaround
  # should go away.
  globals <- list(
    DRAKE_GLOBALS__ = list(
      target = target,
      meta = meta,
      config = config
    )
  )
  if (identical(config$envir, globalenv())){
    # Unit tests should not modify global env # nocov
    if (exists("DRAKE_GLOBALS__", config$envir)){ # nocov # nolint
      warning( # nocov
        "Do not define an object named `DRAKE_GLOBALS__` ", # nocov
        "in the global environment", # nocov
        call. = FALSE # nocov
      ) # nocov
    } # nocov
    globals <- c(globals, as.list(config$envir, all.names = TRUE)) # nocov
  }
  evaluator <- drake_plan_override(
    target = target,
    field = "evaluator",
    config = config
  ) %||%
    future::plan("next")
  announce_build(target = target, meta = meta, config = config)
  structure(
    future::future(
      expr = drake_future_task(
        target = DRAKE_GLOBALS__$target,
        meta = DRAKE_GLOBALS__$meta,
        config = DRAKE_GLOBALS__$config
      ),
      packages = "drake",
      globals = globals,
      evaluator = evaluator
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
  for (worker in workers){
    if (!is_concluded_worker(worker)){
      return(FALSE)
    }
  }
  TRUE
}

running_targets <- function(workers, config){
  lapply(
    X = workers,
    FUN = function(worker){
      if (is_idle(worker)){
        NULL
      } else {
        # It's hard to make this line run in a small test workflow
        # suitable enough for unit testing, but
        # I did artificially stall targets and verified that this line
        # is reached in the future::multisession backend as expected.
        attr(worker, "target") # nocov
      }
    }
  ) %>%
    unlist
}

initialize_workers <- function(){
  out <- list()
  for (i in seq_len(future::nbrOfWorkers()))
    out[[i]] <- empty_worker(target = NA)
  out
}

decrease_revdep_keys <- function(worker, config, queue){
  target <- attr(worker, "target")
  if (!length(target) || is.na(target) || !is.character(target)){
    return()
  }
  revdeps <- dependencies(
    targets = target,
    config = config,
    reverse = TRUE
  ) %>%
    intersect(y = queue$list())
  queue$decrease_key(targets = revdeps)
}

conclude_worker <- function(worker, config, queue){
  decrease_revdep_keys(
    worker = worker,
    queue = queue,
    config = config
  )
  out <- concluded_worker()
  if (is_empty_worker(worker)){
    return(out)
  }
  set_attempt_flag(key = "_attempt", config = config)
  build <- resolve_worker_value(worker = worker, config = config)
  if (config$caching == "worker"){
    return(out)
  }
  conclude_build(
    target = build$target,
    value = build$value,
    meta = build$meta,
    config = config
  )
  out
}

# Also caches error information if available.
# I know, it has a return value AND a side effect,
# but it's hard to think of another clean way
# to handle crashes.
resolve_worker_value <- function(worker, config){
  tryCatch(
    # Check if the worker crashed.
    future::value(worker),
    error = function(e){
      e$message <- paste0(
        "Worker terminated unexpectedly before the target could complete. ",
        "Is something wrong with your system or job scheduler?"
      )
      meta <- list(error = e)
      if (config$caching == "worker"){
        # Need to store the error if the worker crashed.
        handle_build_exceptions(
          target = attr(worker, "target"),
          meta = meta,
          config = config
        )
      }
      # For `caching = "master"`, we need to conclude the build
      # and store the value and metadata.
      list(
        target = attr(worker, "target"),
        value = e,
        meta = meta
      )
    }
  )
}
