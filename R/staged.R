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
      jobs = config$jobs_imports,
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
      jobs = config$jobs_imports
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
      set_attempt_flag(key = "_attempt", config = config)
    }
    prune_envir(targets = stage$targets, config = config, jobs = config$jobs)
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
    stage <- next_stage(config = config, schedule = schedule)
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
  prepare_distributed(config = config)
  schedule <- config$schedule
  while (length(V(schedule)$name)){
    stage <- next_stage(config = config, schedule = schedule)
    schedule <- stage$schedule
    if (!length(stage$targets)){
      break
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
  if (!requireNamespace("clustermq")){
    # nocov start
    drake_error(
      "to use make(parallelism = \"clustermq_staged\"), ",
      "you must install the clustermq package: ",
      "https://github.com/mschubert/clustermq.",
      config = config
    )
    # nocov end
  }
  schedule <- config$schedule
  workers <- clustermq::workers(n_jobs = config$jobs)
  on.exit(workers$finalize())
  while (length(V(schedule)$name)){
    stage <- next_stage(config = config, schedule = schedule)
    schedule <- stage$schedule
    if (!length(stage$targets)){
      break
    } else if (any(stage$targets %in% config$plan$target)){
      set_attempt_flag(key = "_attempt", config = config)
    }
    prune_envir(
      targets = stage$targets,
      config = config,
      jobs = config$jobs_imports
    )
    export <- list()
    if (identical(config$envir, globalenv())){
      export <- as.list(config$envir, all.names = TRUE)
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
      jobs = config$jobs_imports
    )
    builds <- clustermq::Q(
      stage$targets,
      fun = function(target){
        drake::build_clustermq(
          target = target,
          meta_list = meta_list,
          config = config
        )
      },
      workers = workers,
      export = export
    )
    tmp <- lightly_parallelize(
      X = builds,
      FUN = function(build){
        wait_for_file(build = build, config = config)
        conclude_build(
          target = build$target,
          value = build$value,
          meta = build$meta,
          config = config
        )
      },
      jobs = 1 # Could use config$jobs_imports, but memory would blow up.
    )
  }
  invisible()
}

#' @title Build a target using the clustermq backend
#' @description For internal use only
#' @export
#' @keywords internal
#' @inheritParams drake_build
#' @param meta_list list of metadata
build_clustermq <- function(target, meta_list, config){
  do_prework(config = config, verbose_packages = FALSE)
  build <- just_build(
    target = target,
    meta = meta_list[[target]],
    config = config
  )
  if (is_file(target)){
    build$checksum <- rehash_file(target, config = config)
  }
  build
}

wait_for_file <- function(build, config){
  if (!length(build$checksum)){
    return()
  }
  R.utils::withTimeout({
      while (!file.exists(drake_unquote(build$target))){
        Sys.sleep(mc_wait)
      }
      while (!identical(rehash_file(build$target, config), build$checksum)){
        Sys.sleep(mc_wait)
      }
    },
    timeout = 60
  )
}
