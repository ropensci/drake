next_stage <- function(config, schedule) {
  build <- character(0)
  old_leaves <- NULL
  while (TRUE){
    new_leaves <- leaf_nodes(schedule) %>%
      setdiff(y = build)
    console_many_targets(
      targets = new_leaves,
      pattern = "check",
      color = "check",
      config = config
    )
    do_build <- lightly_parallelize(
      X = new_leaves,
      FUN = should_build_target,
      jobs = config$jobs,
      config = config
    ) %>%
      unlist
    build <- c(build, new_leaves[do_build])
    if (all(do_build)){
      break
    } else {
      schedule <- delete_vertices(schedule, v = new_leaves[!do_build])
    }
    old_leaves <- new_leaves
  }
  schedule <- delete_vertices(schedule, v = build)
  list(build = build, schedule = schedule)
}

run_mclapply_staged <- function(config){
  config$jobs <- safe_jobs(config$jobs)
  schedule <- config$schedule
  while(length(V(schedule)$name)){
    stage <- next_stage(config = config, schedule = schedule)
    schedule <- stage$schedule
    if (!length(stage$build)){
      break
    } else if (any(stage$build %in% config$plan$target)){
      set_attempt_flag(config)
    }
    prune_envir(targets = stage$build, config = config)
    tmp <- mclapply(
      X = stage$build,
      FUN = build_and_store,
      mc.cores = config$jobs,
      config = config
    )
  }
  invisible()
}

run_parLapply_staged <- function(config) { # nolint
  if (config$jobs < 2 && !length(config$debug)) {
    return(run_lapply(config = config))
  }
  eval(parse(text = "require(drake)"))
  if (config$jobs < 2 && !length(config$debug)) {
    return(run_lapply(config = config))
  }
  config$workers <- as.character(seq_len(config$jobs))
  console_parLapply(config) # nolint
  config$cluster <- makePSOCKcluster(config$jobs + 1)
  on.exit(stopCluster(cl = config$cluster))
  clusterExport(
    cl = config$cluster, varlist = "config",
    envir = environment())
  if (identical(config$envir, globalenv()) || config$debug){
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
  while(length(V(schedule)$name)){
    stage <- next_stage(config = config, schedule = schedule)
    schedule <- stage$schedule
    if (!length(stage$build)){
      break
    } else if (any(stage$build %in% config$plan$target)){
      set_attempt_flag(config)
    }
    prune_envir(targets = stage$build, config = config)
    if (identical(config$envir, globalenv())){
      clusterCall(
        cl = config$cluster,
        fun = prune_envir,
        targets = stage$build,
        config = config
      )
    }
    tmp <- parLapply(
      cl = config$cluster,
      X = stage$build,
      fun = build_and_store,
      config = config
    )
  }
  invisible()
}
