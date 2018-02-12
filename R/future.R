run_future <- function(config){
  queue <- igraph::topological.sort(config$execution_graph)$name
  workers <- initialize_workers(config)
  # While any targets are queued or running...
  while (work_remains(queue = queue, workers = workers, config = config)){
    for (id in seq_along(workers)){
      if (is_idle(workers[[id]])){
        workers[[id]] <- conclude_worker(
          worker = workers[[id]],
          config = config
        )
        if (!length(queue)){
          next
        }
        next_target <- queue[1]
        next_target_deps <- dependencies(
          targets = next_target, config = config)
        running <- running_targets(workers = workers, config = config)
        running_deps <- intersect(running, next_target_deps)
        if (length(running_deps)){
          next
        }
        queue <- queue[-1]
        protect <- c(running, queue)
        workers[[id]] <- get_redeployment_function(config)(
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

run_future_commands <- run_future_total <- run_future

work_remains <- function(queue, workers, config){
  length(queue) ||
    length(running_targets(workers = workers, config = config))
}

get_redeployment_function <- function(config){
  getFromNamespace(
    x = paste0("worker_", config$parallelism),
    ns = "drake"
  )
}

worker_future_total <- function(id, target, config, protect){
  meta <- drake_meta(target = target, config = config)
  if (!should_build_target(
    target = target,
    meta = meta,
    config = config
  )){
    return(NA)
  }
  prune_envir(
    targets = target, config = config, downstream = protect)
  structure(
    future::future(
      expr = build_and_store(target = target, meta = meta, config = config),
      packages = unique(c(config$packages, (.packages())))
    ),
    target = target
  )
}

worker_future_commands <- function(id, target, config, protect){
  meta <- drake_meta(target = target, config = config)
  if (!should_build_target(
    target = target,
    meta = meta,
    config = config
  )){
    return(NA)
  }
  prune_envir(
    targets = target, config = config, downstream = protect)
  meta$start <- proc.time()
  config$hook(announce_build(target = target, meta = meta, config = config))
  structure(
    future::future(
      expr = config$hook(
        just_build(target = target, meta = meta, config = config)),
      packages = unique(c(config$packages, (.packages())))
    ),
    target = target
  )
}

running_targets <- function(workers, config){
  lightly_parallelize(
    X = workers,
    FUN = function(worker){
      if (identical(worker, NA)){
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
  as.list(rep(NA, length(config$jobs)))
}

# Need to check if the worker quit in error early somehow.
# Maybe the job scheduler failed.
# This should be the responsibility of the `future` package
# or something lower level.
is_idle <- function(worker){
  identical(worker, NA) ||
    future::resolved(worker)
}

# Currently only needed for "future_commands" workers
# since "future_total" workers already conclude the build
# and store the results.
conclude_worker <- function(target, worker, config){
  if (identical(worker, NA)){
    return(NA)
  }
  # We're assuming the "future_commands" backend is not used imports.
  set_attempt_flag(config = config)
  # Here, we should also check if the future resolved due to an error.
  if (config$parallelism != "future_commands"){
    return(NA)
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
  NA
}
