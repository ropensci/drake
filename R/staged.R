next_stage <- function(config, schedule) {
  targets <- character(0)
  old_leaves <- NULL
  meta_list <- list()
  while (TRUE){
    new_leaves <- leaf_nodes(schedule) %>%
      setdiff(y = targets)
    console_many_targets(
      targets = new_leaves,
      pattern = "check",
      color = "check",
      config = config
    )
    new_meta <- lightly_parallelize(
      X = new_leaves,
      FUN = drake_meta,
      jobs = config$jobs,
      config = config
    )
    names(new_meta) <- new_leaves
    do_build <- lightly_parallelize(
      X = new_leaves,
      FUN = function(target){
        should_build_target(
          target = target,
          meta = new_meta[[target]],
          config = config
        )
      },
      jobs = config$jobs
    ) %>%
      unlist
    targets <- c(targets, new_leaves[do_build])
    meta_list <- c(meta_list, new_meta[do_build])
    if (all(do_build)){
      break
    } else {
      schedule <- delete_vertices(schedule, v = new_leaves[!do_build])
    }
    old_leaves <- new_leaves
  }
  schedule <- delete_vertices(schedule, v = targets)
  list(targets = targets, meta_list = meta_list, schedule = schedule)
}

run_mclapply_staged <- function(config){
  config$jobs <- safe_jobs(config$jobs)
  schedule <- config$schedule
  while (length(V(schedule)$name)){
    stage <- next_stage(config = config, schedule = schedule)
    schedule <- stage$schedule
    if (!length(stage$targets)){
      break
    } else if (any(stage$targets %in% config$plan$target)){
      set_attempt_flag(config)
    }
    prune_envir(targets = stage$targets, config = config)
    tmp <- mclapply(
      X = stage$targets,
      FUN = function(target){
        build_and_store(
          target = target,
          meta = stage$meta_list[[target]],
          config = config
        )
      },
      mc.cores = config$jobs
    )
  }
  invisible()
}

run_parLapply_staged <- function(config) { # nolint
  if (config$jobs < 2 && !length(config$debug)) {
    return(run_lapply(config = config))
  }
  eval(parse(text = "require(drake)"))
  config$workers <- as.character(seq_len(config$jobs))
  console_parLapply(config) # nolint
  config$cluster <- makePSOCKcluster(config$jobs)
  on.exit(stopCluster(cl = config$cluster))
  clusterExport(
    cl = config$cluster, varlist = "config",
    envir = environment())
  if (identical(config$envir, globalenv()) || length(config$debug)){
    clusterExport(
      cl = config$cluster,
      varlist = ls(globalenv(),
      all.names = TRUE),
      envir = globalenv()
    )
  }
  clusterCall(cl = config$cluster, fun = function(){
    eval(parse(text = "require(drake)"))
  })
  clusterCall(
    cl = config$cluster,
    fun = do_prework,
    config = config,
    verbose_packages = FALSE
  )
  schedule <- config$schedule
  while (length(V(schedule)$name)){
    stage <- next_stage(config = config, schedule = schedule)
    schedule <- stage$schedule
    if (!length(stage$targets)){
      break
    } else if (any(stage$targets %in% config$plan$target)){
      set_attempt_flag(config)
    }
    prune_envir(targets = stage$targets, config = config)
    if (identical(config$envir, globalenv())){
      # Regular unit tests should not modify the global environment.
      # Tests in tests/scenarios/all.R cover these lines.
      # nocov start
      clusterCall(
        cl = config$cluster,
        fun = prune_envir,
        targets = stage$targets,
        config = config
      )
      # nocov end
    }
    tmp <- parLapply(
      cl = config$cluster,
      X = stage$targets,
      fun = function(target){
        build_and_store(
          target = target,
          meta = stage$meta_list[[target]],
          config = config
        )
      }
    )
  }
  invisible()
}
