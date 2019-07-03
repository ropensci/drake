config_checks <- function(config) {
  if (identical(config$skip_safety_checks, TRUE)) {
    return(invisible())
  }
  check_case_sensitivity(config)
  check_drake_graph(graph = config$graph)
  cache_vers_stop(config$cache)
  check_parallelism(config$parallelism, config$jobs)
  check_jobs(config$jobs)
}

plan_checks <- function(plan) {
  stopifnot(is.data.frame(plan))
  if (!all(c("target", "command") %in% colnames(plan))) {
    stop(
      "The columns of your workflow plan data frame ",
      "must include 'target' and 'command'.",
      call. = FALSE
    )
  }
  if (any(bad_symbols %in% plan$target)) {
    stop(
      "symbols that cannot be target names: \n",
      multiline_message(shQuote(bad_symbols)),
      call. = FALSE
    )
  }
}

runtime_checks <- function(config) {
  if (identical(config$skip_safety_checks, TRUE)) {
    return(invisible())
  }
  missing_input_files(config = config)
  subdirectory_warning(config = config)
  assert_outside_cache(config = config)
}

missing_input_files <- function(config) {
  files <- parallel_filter(
    all_imports(config),
    f = is_encoded_path,
    jobs = config$jobs_preprocess
  )
  files <- decode_path(x = files, config = config)
  missing_files <- files[!file_dep_exists(files)]
  if (length(missing_files)) {
    warning(
      "missing input files:\n",
      multiline_message(missing_files),
      call. = FALSE
    )
  }
  invisible()
}

check_case_sensitivity <- function(config) {
  x <- igraph::V(config$graph)$name
  x <- x[!is_encoded_path(x) & !is_encoded_namespaced(x)]
  lower <- tolower(x)
  i <- duplicated(lower)
  if (!any(i)) {
    return()
  }
  dups <- sort(x[which(lower %in% lower[i])])
  warning(
    "Duplicated target/import names when converting to lowercase:\n",
    multiline_message(dups),
    "\nDuplicates cause problems on Windows ",
    "because the file system is case insensitive. Options:\n",
    "  (1) Make your target/import names more unique.\n",
    "  (2) Use a more portable custom cache, ",
    "e.g. make(cache = storr::storr_rds(mangle_key = TRUE))\n",
    "  (3) Avoid Windows.",
    call. = FALSE
  )
}

check_drake_graph <- function(graph) {
  if (is_dag(graph)) {
    return()
  }
  comp <- igraph::components(graph, mode = "strong")
  cycle_component_indices <- which(comp$csize > 1)
  cycles <- lapply(cycle_component_indices, function(i) {
    out <- names(comp$membership[comp$membership == i])
    out <- paste(out, collapse = " ")
  })
  cycles <- unlist(cycles)
  stop(
    "Circular workflow:\n",
    "  at least one target in your drake plan\n",
    "  ultimately depends on itself.\n",
    "If you believe a dependency was detected in error\n",
    "  (example: https://github.com/ropensci/drake/issues/578)\n",
    "  then consider using ignore() to mask sections \n",
    "  of your commands or imported functions.\n",
    "Cycles:\n",
    multiline_message(cycles),
    call. = FALSE
  )
}

check_jobs <- function(jobs) {
  stopifnot(length(jobs) > 0)
  stopifnot(is.numeric(jobs) || is.integer(jobs))
  stopifnot(all(jobs > 0))
  if (length(jobs) > 1) {
    stop(
      "The `jobs` argument of `make()` should be of length 1. ",
      "Use the `jobs_preprocess` argument to parallelize the imports ",
      "and other preprocessing tasks.",
      call. = FALSE
    )
  }
}

check_parallelism <- function(parallelism, jobs) {
  stopifnot(is.character(parallelism) || is.function(parallelism))
  stopifnot(length(parallelism) > 0)
  if (length(parallelism) > 1) {
    stop(
      "The `parallelism` argument of `make()` should be of length 1.",
      call. = FALSE
    )
  }
  if (identical(parallelism, "loop") && jobs > 1L) {
    warning(
      "In make(), `parallelism` should not be \"loop\" if `jobs` > 1",
      call. = FALSE
    )
  }
}

check_make_call <- function(call) {
  x <- names(call)
  if ("config" %in% names(call) && sum(nzchar(x)) > 1L) {
    warning(
      "if you supply a ", shQuote("config"),
      " argument to ", shQuote("make()"),
      " then all additional arguments are ignored. ",
      "For example, in ", shQuote("make(config = config, verbose = 0L)"),
      "verbosity remains at ", shQuote("config$verbose"), ".",
      call. = FALSE
    )
  }
}

subdirectory_warning <- function(config) {
  if (identical(Sys.getenv("drake_warn_subdir"), "false")) {
    return()
  }
  dir_cache <- config$cache$driver$path
  if (is.null(dir_cache)) {
    return()
  }
  if (!identical(basename(dir_cache), basename(default_cache_path()))) {
    return()
  }
  dir_wd <- getwd()
  in_root <- is.null(dir_cache) ||
    basename(dir_cache) %in% list.files(path = dir_wd, all.files = TRUE)
  if (in_root) {
    return()
  }
  warning(
    "Running make() in a subdirectory of your project. \n",
    "This could cause problems if your ",
    "file_in()/file_out()/knitr_in() files ",
    "are relative paths.\n",
    "Please either\n",
    "  (1) run make() from your drake project root, or\n",
    "  (2) create a cache in your working ",
    "directory with new_cache('path_name'), or\n",
    "  (3) supply a cache of your own (e.g. make(cache = your_cache))\n",
    "      whose folder name is not '.drake'.\n",
    "  running make() from: ", dir_wd, "\n",
    "  drake project root:  ", dirname(dir_cache), "\n",
    "  cache directory:     ", dir_cache,
    call. = FALSE
  )
}

assert_outside_cache <- function(config) {
  work_dir <- normalizePath(getwd(), mustWork = FALSE)
  cache_dir <- normalizePath(config$cache_path, mustWork = FALSE)
  if (identical(work_dir, cache_dir)) {
    stop(
      "cannot run make() from inside the cache: ", shQuote(cache_dir),
      ". The cache path must be different from your working directory. ",
      "If your drake project lives at ", shQuote("/your/project/root/"), # nolint
      " then you should run ", shQuote("make()"), " from this directory, ",
      "and your cache should be in a subfolder, e.g. ",
      shQuote("/your/project/root/.drake/") # nolint
    )
  }
}
