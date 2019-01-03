process_import <- function(target, meta, config) {
  if (meta$isfile) {
    value <- NA_character_
    is_missing <- !file.exists(decode_path(target, config))
  } else {
    value <- get_import_from_memory(target, config = config)
    is_missing <- identical(value, NA_character_)
  }
  if (is_missing) {
    console(imported = NA_character_, target = target, config = config)
  }
  store_single_output(
    target = target,
    value = value,
    meta = meta,
    config = config
  )
}

process_imports_mclapply <- function(config) {
  if (config$jobs > 1L) {
    assert_pkg("parallel")
  }
  schedule <- config$schedule
  while (length(V(schedule)$name)) {
    imports <- leaf_nodes(schedule)
    safe_mclapply(
      X = imports,
      FUN = process_import,
      config = config,
      mc.cores = config$jobs_preprocess
    )
    schedule <- delete_vertices(schedule, v = imports)
  }
  invisible()
}

process_imports_parLapply <- function(config) { # nolint
  assert_pkg("parallel")
  eval(parse(text = "require(drake)"))
  console_parLapply(config) # nolint
  config$cluster <- parallel::makePSOCKcluster(config$jobs_preprocess)
  on.exit(parallel::stopCluster(cl = config$cluster))
  parallel::clusterExport(
    cl = config$cluster, varlist = "config",
    envir = environment())
  if (identical(config$envir, globalenv()) || length(config$debug)) {
    parallel::clusterExport(
      cl = config$cluster,
      varlist = ls(globalenv(),
                   all.names = TRUE),
      envir = globalenv()
    )
  }
  parallel::clusterCall(
    cl = config$cluster,
    fun = function() {
      eval(parse(text = "suppressPackageStartupMessages(require(drake))"))
    }
  )
  parallel::clusterCall(
    cl = config$cluster,
    fun = do_prework,
    config = config,
    verbose_packages = FALSE
  )
  schedule <- config$schedule
  while (length(V(schedule)$name)) {
    imports <- leaf_nodes(schedule)
    parallel::parLapply(
      cl = config$cluster,
      X = imports,
      fun = process_import,
      config = config
    )
    schedule <- delete_vertices(schedule, v = imports)
  }
  invisible()
}
