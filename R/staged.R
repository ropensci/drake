next_stage <- function(config, schedule, jobs) {
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
      jobs = jobs,
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
      jobs = jobs
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
    stage <- next_stage(
      config = config,
      schedule = schedule,
      jobs = config$jobs
    )
    schedule <- stage$schedule
    if (!length(stage$targets)){
      break
    } else if (any(stage$targets %in% config$plan$target)){
      set_attempt_flag(key = "_attempt", config = config)
    }
    prune_envir(targets = stage$targets, config = config, jobs = config$jobs)
    parallel::mclapply(
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
    return(run_loop(config = config))
  }
  eval(parse(text = "require(drake)"))
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
    stage <- next_stage(
      config = config,
      schedule = schedule,
      jobs = config$jobs
    )
    schedule <- stage$schedule
    if (!length(stage$targets)){
      break
    } else if (any(stage$targets %in% config$plan$target)){
      set_attempt_flag(key = "_attempt", config = config)
    }
    prune_envir(targets = stage$targets, config = config, jobs = config$jobs)
    if (identical(config$envir, globalenv())){
      # Regular unit tests should not modify the global environment.
      # Tests in tests/scenarios/all.R cover these lines.
      # nocov start
      clusterCall(
        cl = config$cluster,
        fun = function(targets, config) {
          config$verbose <- FALSE
          suppressWarnings(
            drake::prune_envir(targets = targets, config = config)
          )
        },
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

run_future_lapply_staged <- function(config){
  assert_pkg("future")
  assert_pkg("future.apply")
  prepare_distributed(config = config)
  schedule <- config$schedule
  while (length(V(schedule)$name)){
    stage <- next_stage(
      config = config,
      schedule = schedule,
      jobs = config$jobs_imports
    )
    schedule <- stage$schedule
    if (!length(stage$targets)){
      # Keep in case outdated targets are ever back in the schedule.
      break # nocov
    } else if (any(stage$targets %in% config$plan$target)){
      set_attempt_flag(key = "_attempt", config = config)
    }
    tmp <- future.apply::future_lapply(
      X = stage$targets,
      FUN = build_distributed,
      cache_path = config$cache_path,
      check = FALSE
    )
  }
  invisible()
}

run_clustermq_staged <- function(config){
  assert_pkg("clustermq", version = "0.8.4.99")
  schedule <- config$schedule
  workers <- NULL
  while (length(V(schedule)$name)){
    stage <- next_stage(
      config = config,
      schedule = schedule,
      jobs = 1 # config$jobs # nolint
    )
    schedule <- stage$schedule
    if (!length(stage$targets)){
      # Keep in case outdated targets are ever back in the schedule.
      break # nocov
    } else if (is.null(workers)){
      workers <- clustermq::workers(
        n_jobs = config$jobs,
        template = config$template
      )
      on.exit(workers$finalize())
    }
    if (any(stage$targets %in% config$plan$target)){
      set_attempt_flag(key = "_attempt", config = config)
    }
    prune_envir(
      targets = stage$targets,
      config = config,
      jobs = 1 # config$jobs_imports # nolint
    )
    export <- list()
    if (identical(config$envir, globalenv())){
      export <- as.list(config$envir, all.names = TRUE) # nocov
    }
    export$config <- config
    export$meta_list <- stage$meta_list
    meta_list <- NULL
    tmp <- lightly_parallelize(
      X = stage$targets,
      FUN = function(target){
        announce_build(
          target = target,
          meta = stage$meta_list[[target]],
          config = config
        )
      },
      jobs = 1 # config$jobs_imports # nolint
    )
    builds <- clustermq::Q(
      stage$targets,
      fun = function(target){
        # This call is actually tested in tests/testthat/test-clustermq.R.
        # nocov start
        drake::cmq_staged_build(
          target = target,
          meta_list = meta_list,
          config = config
        )
        # nocov end
      },
      workers = workers,
      export = export
    )
    lightly_parallelize(
      X = builds,
      FUN = function(build){
        mc_wait_outfile_checksum(
          target = build$target,
          checksum = build$checksum,
          config = config
        )
        conclude_build(
          target = build$target,
          value = build$value,
          meta = build$meta,
          config = config
        )
      },
      jobs = 1 # config$jobs_imports # nolint
    )
  }
  if (!is.null(workers) && workers$cleanup()){
    on.exit()
  }
  invisible()
}

#' @title Build a target using the clustermq_staged backend
#' @description For internal use only
#' @export
#' @keywords internal
#' @inheritParams drake_build
#' @param meta_list list of metadata
cmq_staged_build <- function(target, meta_list, config){
  # This function is actually tested in tests/testthat/test-clustermq.R.
  # nocov start
  do_prework(config = config, verbose_packages = FALSE)
  meta_list[[target]]$start <- proc.time()
  build <- just_build(
    target = target,
    meta = meta_list[[target]],
    config = config
  )
  build$checksum <- mc_output_file_checksum(target, config)
  build
  # nocov end
}
