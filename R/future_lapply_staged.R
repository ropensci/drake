
next_stage <- function(config, schedule, jobs) {
  targets <- character(0)
  old_leaves <- NULL
  meta_list <- list()
  while (TRUE) {
    new_leaves <- leaf_nodes(schedule)
    new_leaves <- setdiff(new_leaves, targets)
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
      FUN = function(target) {
        should_build_target(
          target = target,
          meta = new_meta[[target]],
          config = config
        )
      },
      jobs = jobs
    )
    do_build <- unlist(do_build)
    targets <- c(targets, new_leaves[do_build])
    meta_list <- c(meta_list, new_meta[do_build])
    if (all(do_build)) {
      break
    } else {
      schedule <- delete_vertices(schedule, v = new_leaves[!do_build])
    }
    old_leaves <- new_leaves
  }
  schedule <- delete_vertices(schedule, v = targets)
  list(targets = targets, meta_list = meta_list, schedule = schedule)
}

run_future_lapply_staged <- function(config) {
  assert_pkg("future")
  assert_pkg("future.apply")
  prepare_distributed(config = config)
  schedule <- config$schedule
  while (length(V(schedule)$name)) {
    stage <- next_stage(
      config = config,
      schedule = schedule,
      jobs = config$jobs_preprocess
    )
    schedule <- stage$schedule
    if (!length(stage$targets)) {
      # Keep in case outdated targets are ever back in the schedule.
      break # nocov
    } else if (any(stage$targets %in% config$plan$target)) {
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
