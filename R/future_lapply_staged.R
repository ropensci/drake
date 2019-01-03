run_future_lapply_staged <- function(config) {
  assert_pkg("future")
  assert_pkg("future.apply")
  fls_prepare(config = config)
  schedule <- config$schedule
  while (length(V(schedule)$name)) {
    set_attempt_flag(key = "_attempt", config = config)
    targets <- leaf_nodes(schedule)
    future.apply::future_lapply(
      X = targets,
      FUN = fls_build,
      cache_path = config$cache_path
    )
    schedule <- delete_vertices(schedule, v = targets)
  }
  fls_conclude(config)
  invisible()
}

fls_prepare <- function(config) {
  if (!file.exists(config$cache_path)) {
    dir.create(config$cache_path)
  }
  save(
    list = setdiff(ls(globalenv(), all.names = TRUE), config$plan$target),
    envir = globalenv(),
    file = globalenv_file(config$cache_path)
  )
  for (item in c("envir", "schedule")) {
    config$cache$set(key = item, value = config[[item]], namespace = "config")
  }
  invisible()
}

fls_build <- function(target, cache_path) {
  config <- recover_drake_config(cache_path = cache_path)
  eval(parse(text = "base::require(drake, quietly = TRUE)"))
  do_prework(config = config, verbose_packages = FALSE)
  manage_memory(targets = target, config = config)
  meta <- drake_meta(target = target, config = config)
  announce_build(target = target, meta = meta, config = config)
  build <- build_target(target = target, meta = meta, config = config)
  conclude_build(
    target = target,
    value = build$value,
    meta = build$meta,
    config = config
  )
  invisible()
}

fls_conclude <- function(config) {
  dir <- cache_path(config$cache)
  file <- globalenv_file(dir)
  unlink(file, force = TRUE)
}

recover_drake_config <- function(cache_path) {
  cache <- this_cache(cache_path, verbose = FALSE)
  config <- read_drake_config(cache = cache)
  dir <- cache_path(cache = cache)
  file <- globalenv_file(dir)
  load(file = file, envir = globalenv())
  config
}

globalenv_file <- function(cache_path) {
  file.path(cache_path, "globalenv.RData")
}
