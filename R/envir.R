assign_to_envir <- Vectorize(
  function(target, value, config){
    if (is_file(target) | !(target %in% config$plan$target)){
      return()
    }
    assign(x = target, value = value, envir = config$envir)
  },
  c("target", "value")
  )

prune_envir <- function(targets, config){
  downstream <- lightly_parallelize(
    targets,
    function(vertex){
      subcomponent(config$graph, v = vertex, mode = "out")$name
    },
    jobs = config$jobs
  ) %>%
    unlist() %>%
    unique()
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
    Filter(f = is_not_file) %>%
    intersect(y = already_loaded)
  if (length(discard_these)){
    console_many_targets(
      discard_these,
      message = "unload",
      config = config
    )
    rm(list = discard_these, envir = config$envir)
  }
  if (length(load_these)){
    console_many_targets(
      load_these,
      message = "load",
      config = config
    )
    loadd(list = load_these, envir = config$envir, cache = config$cache,
          verbose = FALSE)
  }
  invisible()
}

nonfile_target_dependencies <- function(targets, config){
  dependencies(targets = targets, config = config) %>%
    Filter(f = is_not_file) %>%
    intersect(y = config$plan$target)
}
