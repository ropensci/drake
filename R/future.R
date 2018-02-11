run_future <- function(config){
  targets <- igraph::topological.sort(config$execution_graph)$name
  workers <- initialize_workers(config)
  while (length(targets)){
    for (id in seq_along(workers)){
      if (!length(targets)){
        break
      }
      if (resolved_correctly(workers[[id]])){
        workers[[id]] <- conclude_worker(
          worker = workers[[id]],
          config = config
        )
        next_target <- targets[1]
        next_target_deps <- dependencies(targets = next_target, config = config)
        running <- running_targets(workers = workers, config = config)
        running_deps <- intersect(running, next_target_deps)
        if (length(running_deps)){
          next
        }
        targets <- targets[-1]
        downstream <- c(running, targets)
        workers[[id]] <- get_redeployment_function(config)(
          id = id,
          target = next_target,
          config = config,
          downstream = downstream
        )
      }
    }
    Sys.sleep(1e-9)
  }
}

run_future_commands <- run_future_total <- run_future

get_redeployment_function <- function(config){
  getFromNamespace(
    x = paste0("worker_", config$parallelism),
    ns = "drake"
  )
}

worker_future_total <- function(id, target, config, downstream){
  meta <- drake_meta(target = target, config = config)
  if (!should_build_target(
    target = target,
    meta = meta,
    config = config
  )){
    return(NA)
  }
  prune_envir(
    targets = target, config = config, downstream = downstream)
  structure(
    future::future(
      expr = build_and_store(target = target, meta = meta, config = config),
      packages = unique(c(config$packages, (.packages())))
    ),
    target = target
  )
}

worker_future_commands <- function(id, target, config, downstream){
  meta <- drake_meta(target = target, config = config)
  if (!should_build_target(
    target = target,
    meta = meta,
    config = config
  )){
    return(NA)
  }
  prune_envir(
    targets = target, config = config, downstream = downstream)
  meta$start <- proc.time()
  config$hook(announce_build(target = target, meta = meta, config = config))
  structure(
    future::future(
      expr = just_build(target = target, meta = meta, config = config),
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
resolved_correctly <- function(worker){
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
