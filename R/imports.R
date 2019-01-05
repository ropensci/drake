process_import <- function(import, config) {
  meta <- drake_meta(target = import, config = config)
  if (meta$isfile) {
    value <- NA_character_
    is_missing <- !file.exists(decode_path(import, config))
  } else {
    value <- get_import_from_memory(import, config = config)
    is_missing <- identical(value, NA_character_)
  }
  if (is_missing) {
    console_missing(target = import, config = config)
  } else {
    console_import(target = import, config = config)
  }
  store_single_output(
    target = import,
    value = value,
    meta = meta,
    config = config
  )
}

process_imports_mclapply <- function(config) {
  if (config$jobs_preprocess > 1L) {
    assert_pkg("parallel")
  }
  while (length(V(config$imports)$name)) {
    imports <- leaf_nodes(config$imports)
    lightly_parallelize(
      X = imports,
      FUN = process_import,
      config = config,
      jobs = config$jobs_preprocess
    )
    config$imports <- delete_vertices(config$imports, v = imports)
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
    # nocov start
    parallel::clusterExport(
      cl = config$cluster,
      varlist = ls(globalenv(), all.names = TRUE),
      envir = globalenv()
    )
    # nocov end
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
  while (length(V(config$imports)$name)) {
    imports <- leaf_nodes(config$imports)
    parallel::parLapply(
      cl = config$cluster,
      X = imports,
      fun = process_import,
      config = config
    )
    config$imports <- delete_vertices(config$imports, v = imports)
  }
  invisible()
}
