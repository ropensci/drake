process_imports <- function(config) {
  if (on_windows() && config$settings$jobs_preprocess > 1L) {
    process_imports_parLapply(config) # nocov
  } else {
    process_imports_mclapply(config)
  }
}

#' @title Process an imported data object
#' `r lifecycle::badge("stable")`
#' @description For internal use only. Not a user-side function.
#' @export
#' @keywords internal
#' @param import Character, name of an import to process
#' @param config [drake_config()] object
process_import <- function(import, config) {
  meta <- drake_meta_(import, config)
  if (meta$isfile) {
    value <- NA_character_
    path <- config$cache$decode_path(import)
    is_missing <- !file.exists(path) && !is_url(path)
  } else {
    value <- get_import_from_memory(import, config = config)
    is_missing <- identical(value, NA_character_)
  }
  if (is_missing) {
    config$logger$disk(
      "missing",
      target = config$cache$display_keys(import),
      color = "missing"
    )
  } else {
    config$logger$disk("import", target = config$cache$display_keys(import))
  }
  store_item(
    target = import,
    value = value,
    meta = meta,
    config = config
  )
}

get_import_from_memory <- function(target, config) {
  if (is_encoded_path(target)) {
    return(NA_character_)
  }
  if (is_encoded_namespaced(target)) {
    target <- config$cache$decode_namespaced(target)
  }
  if (exists(x = target, envir = config$envir, inherits = FALSE)) {
    return(get(x = target, envir = config$envir, inherits = FALSE))
  }
  parsed <- parse(text = target)
  parsed <- as.call(parsed)
  parsed <- as.list(parsed)
  lang <- parsed[[1]]
  is_namespaced <- length(lang) > 1
  if (is_namespaced) {
    stopifnot(safe_deparse(lang[[1]], backtick = FALSE) %in% c("::", ":::"))
    pkg <- safe_deparse(lang[[2]], backtick = FALSE)
    fun <- safe_deparse(lang[[3]], backtick = FALSE)
    tryCatch(get(fun, envir = getNamespace(pkg)), error = error_na)
  } else {
    NA_character_
  }
}

process_imports_mclapply <- function(config) {
  imports_graph <- subset_graph(config$graph, all_imports(config))
  while (length(V(imports_graph)$name)) {
    imports <- leaf_nodes(imports_graph)
    lightly_parallelize(
      X = imports,
      FUN = drake::process_import,
      config = config,
      jobs = config$settings$jobs_preprocess
    )
    imports_graph <- delete_vertices(imports_graph, v = imports)
  }
  invisible()
}

process_imports_parLapply <- function(config) { # nolint
  config$logger$disk(
    "load parallel socket cluster with",
    config$settings$jobs_preprocess,
    "workers"
  )
  config$cluster <- parallel::makePSOCKcluster(config$settings$jobs_preprocess)
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
    fun = drake::do_prework,
    config = config,
    verbose_packages = FALSE
  )
  imports_graph <- subset_graph(config$graph, all_imports(config))
  while (length(V(imports_graph)$name)) {
    imports <- leaf_nodes(imports_graph)
    parallel::parLapply(
      cl = config$cluster,
      X = imports,
      fun = function(import, config) {
        drake::process_import(import = import, config = config)
      },
      config = config
    )
    imports_graph <- delete_vertices(imports_graph, v = imports)
  }
  invisible()
}
