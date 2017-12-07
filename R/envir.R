assign_to_envir <- Vectorize(
  function(target, value, config){
    if (config$lazy_load){
      return()
    }
    if (is_file(target) | !(target %in% config$plan$target)){
      return()
    }
    assign(x = target, value = value, envir = config$envir)
  },
  c("target", "value")
  )

prune_envir <- function(targets, config){
  downstream <- downstream_nodes(
    from = targets, graph = config$graph, jobs = config$jobs)
  already_loaded <- ls(envir = config$envir, all.names = TRUE) %>%
    intersect(y = config$plan$target)
  load_these <- nonfile_target_dependencies(
    targets = targets,
    config = config
    ) %>%
    setdiff(y = c(targets, already_loaded))
  keep_these <- nonfile_target_dependencies(
    targets = downstream,
    config = config
  )
  discard_these <- setdiff(x = config$plan$target, y = keep_these) %>%
    filter_non_files(jobs = config$jobs) %>%
    intersect(y = already_loaded)
  if (length(discard_these)){
    console_many_targets(
      discard_these,
      pattern = "unload",
      config = config
    )
    rm(list = discard_these, envir = config$envir)
  }
  if (length(load_these)){
    if (!config$lazy_load){
      console_many_targets(
        load_these,
        pattern = "load",
        config = config
      )
    }
    loadd(list = load_these, envir = config$envir, cache = config$cache,
      verbose = FALSE, lazy = config$lazy_load)
  }
  invisible()
}

nonfile_target_dependencies <- function(targets, config){
  deps <- dependencies(targets = targets, config = config)
  out <- filter_non_files(x = deps, jobs = config$jobs)
  intersect(out, config$plan$target)
}

filter_non_files <- function(x, jobs){
  non_files <- lightly_parallelize(
    X = x,
    FUN = is_not_file,
    jobs = jobs
  ) %>%
    as.logical
  x[non_files]
}
