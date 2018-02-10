run_future <- function(config){
  targets <- igraph::topological.sort(config$graph)$name
  workers <- initialize_workers(config)
  while (length(targets)){
    for (id in seq_along(workers)){
      if (!length(targets)){
        break
      }
      if (resolved_correctly(workers[[id]])){
        conclude_worker(worker = workers[[id]], config = config)
        workers[[id]] <- get_redeployment_function(config)(
          id = id, target = targets[1], config = config)
        targets <- targets[-1]
      }
    }
    Sys.sleep(1e-9)
  }
}

run_future_commands <- run_future_caching <- run_future

get_redeployment_function <- function(config){
  getFromNamespace(
    x = paste0("worker_", config$parallelism)
    ns = "drake"
  )
}

worker_future_commands <- function(id, target, config){
  meta <- drake_meta(target = target, config = config)
  if (!should_build_target(
    target = target,
    meta = meta,
    config = config
  )){
    return(NA)
  }
  config$cache$set(
    key = as.character(id),
    value = target,
    namespace = "workers"
  )
  prune_envir(targets = running_targets(config), config = config)
  future::future(
    expr = build_and_store(target = target, meta = meta, config = config),
    packages = unique(c(config$packages, (.packages())))
  )
}

worker_future_total <- function(id, target, config){
  meta <- drake_meta(target = target, config = config)
  if (!should_build_target(
    target = target,
    meta = meta,
    config = config
  )){
    return(NA)
  }
  config$cache$set(
    key = as.character(id),
    value = target,
    namespace = "workers"
  )
  prune_envir(targets = running_targets(config), config = config)
  meta$start <- proc.time()
  config$hook(announce_build(target = target, meta = meta, config = config))
  future(
    expr = just_build(target = target, meta = meta, config = config),
    packages = unique(c(config$packages, (.packages())))
  )
}

running_targets <- function(config){
  lightly_parallelize(
    X = config$cache$list(namespace = "workers"),
    FUN = function(key){
      config$cache$get(key = key, namespace = "workers")
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
resolved_correctly <- function(worker){
  future::resolved(worker)
}

# Currently only needed for "future_commands" workers
# since "future_total" workers already conclude the build
# and store the results.
conclude_worker(target, worker, config){
  if (config$parallelism == "future_commands"){
    config$hook({
      build <- future::value(worker)
      conclude_build(
        target = build$target,
        value = build$value,
        meta = build$meta,
        config = config
      )
    })
  }
}
