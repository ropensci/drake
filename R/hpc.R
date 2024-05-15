#' @title Write a template file for deploying
#'   work to a cluster / job scheduler.
#' `r lifecycle::badge("stable")`
#' @description See the example files from
#'  [drake_examples()] and [drake_example()]
#'   for example usage.
#' @export
#' @seealso [drake_hpc_template_files()],
#'   [drake_examples()], [drake_example()],
#'   [shell_file()]
#' @return `NULL` is returned,
#'   but a batchtools template file is written.
#' @param file Name of the template file, including the "tmpl" extension.
#' @param to Character vector, where to write the file.
#' @param overwrite Logical, whether to overwrite an existing file of the
#'   same name.
#' @examples
#' \dontrun{
#' plan <- drake_plan(x = rnorm(1e7), y = rnorm(1e7))
#' # List the available template files.
#' drake_hpc_template_files()
#' # Write a SLURM template file.
#' out <- file.path(tempdir(), "slurm_batchtools.tmpl")
#' drake_hpc_template_file("slurm_batchtools.tmpl", to = tempdir())
#' cat(readLines(out), sep = "\n")
#' # library(future.batchtools) # nolint
#' # future::plan(batchtools_slurm, template = out) # nolint
#' # make(plan, parallelism = "future", jobs = 2) # nolint
#' }
drake_hpc_template_file <- function(
  file = drake::drake_hpc_template_files(),
  to = getwd(),
  overwrite = FALSE
) {
  file <- match.arg(file)
  dir <- system.file(
    file.path("templates", "hpc"),
    package = "drake",
    mustWork = TRUE
  )
  file.copy(
    from = file.path(dir, file),
    to = to,
    overwrite = overwrite,
    recursive = TRUE
  )
  invisible()
}

#' @title List the available example template files for deploying
#'   work to a cluster / job scheduler.
#' `r lifecycle::badge("stable")`
#' @description See the example files from
#'  [drake_examples()] and [drake_example()]
#'   for example usage.
#' @export
#' @seealso [drake_hpc_template_file()],
#'   [drake_examples()], [drake_example()],
#'   [shell_file()]
#' @return A character vector of example template files that
#'   you can write with [drake_hpc_template_file()].
#' @examples
#' \dontrun{
#' plan <- drake_plan(x = rnorm(1e7), y = rnorm(1e7))
#' # List the available template files.
#' drake_hpc_template_files()
#' # Write a SLURM template file.
#' out <- file.path(tempdir(), "slurm_batchtools.tmpl")
#' drake_hpc_template_file("slurm_batchtools.tmpl", to = tempdir())
#' cat(readLines(out), sep = "\n")
#' # library(future.batchtools) # nolint
#' # future::plan(batchtools_slurm, template = out) # nolint
#' # make(plan, parallelism = "future", jobs = 2) # nolint
#' }
drake_hpc_template_files <- function() {
  dir(
    system.file(
      file.path("templates", "hpc"),
      package = "drake",
      mustWork = TRUE
    )
  )
}

no_hpc <- function(target, config) {
  identical(config$spec[[target]]$hpc, FALSE) ||
    is_dynamic(target, config)
}

hpc_caching <- function(target, config) {
  out <- config$spec[[target]]$caching
  if (is.null(out) || is.na(out)) {
    out <- config$caching
  }
  if (identical(out, "master")) {
    warn0(
      "caching = \"master\" is deprecated. ",
      "Use caching = \"main\" instead."
    )
    out <- "main"
  }
  match.arg(out, choices = c("main", "worker"))
}

hpc_config <- function(config) {
  discard <- c(
    "imports",
    "spec",
    "plan",
    "targets",
    "trigger"
  )
  for (x in discard) {
    config[[x]] <- NULL
  }
  config$cache$flush_cache()
  config
}

get_hpc_config_tmp <- function(config) {
  list(
    ht_is_subtarget = config$ht_is_subtarget,
    ht_subtarget_parents = config$ht_subtarget_parents
  )
}

restore_hpc_config_tmp <- function(tmp, config) {
  config$ht_is_subtarget <- tmp$ht_is_subtarget
  config$ht_subtarget_parents <- tmp$ht_subtarget_parents
  config
}

hpc_spec <- function(target, config) {
  class(target) <- ifelse(is_subtarget(target, config), "subtarget", "target")
  hpc_spec_impl(target, config)
}

hpc_spec_impl <- function(target, config) {
  UseMethod("hpc_spec_impl")
}

#' @export
hpc_spec_impl.subtarget <- function(target, config) {
  spec <- new.env(parent = emptyenv())
  parent <- config$spec[[target]]$subtarget_parent
  dynamic_deps <- config$spec[[target]]$deps_dynamic
  keys <- c(target, parent, dynamic_deps)
  for (key in keys) {
    assign(key, config$spec[[key]], envir = spec, inherits = FALSE)
  }
  spec
}

#' @export
hpc_spec_impl.default <- function(target, config) {
  spec <- new.env(parent = emptyenv())
  assign(target, config$spec[[target]], envir = spec, inherits = FALSE)
  spec
}

wait_outfile_checksum <- function(
  target,
  value,
  checksum,
  config,
  timeout = 300
) {
  wait_checksum(
    target = target,
    value = value,
    checksum = checksum,
    config = config,
    timeout = timeout,
    criterion = is_good_outfile_checksum
  )
}

wait_checksum <- function(
  target,
  value,
  checksum,
  config,
  timeout = 300,
  criterion = is_good_checksum
) {
  i <- 0
  time_left <- timeout
  while (time_left > 0) {
    if (criterion(target, value, checksum, config)) {
      return()
    } else {
      sleep <- config$settings$sleep(max(0L, i))
      Sys.sleep(sleep)
      time_left <- time_left - sleep
    }
    i <- i + 1
  }
  msg <- paste0(
    "Target `", target, "` did not download from your ",
    "network file system. Checksum verification timed out after about ",
    timeout, " seconds."
  )
  config$logger$disk(paste("Error:", msg))
  stop0(msg)
}

is_good_checksum <- function(target, value, checksum, config) {
  # Not actually reached, but may come in handy later.
  # nocov start
  if (!length(checksum)) {
    warn_no_checksum(target = target, config = config)
    return(TRUE)
  }
  if (identical("failed", config$cache$get_progress(target))) {
    return(TRUE) # covered with parallel processes # nocov
  }
  # nocov end
  local_checksum <- get_checksum(target, value, config)
  if (!identical(local_checksum, checksum)) {
    return(FALSE)
  }
  out <- all(
    vapply(
      X = unlist(strsplit(local_checksum, " "))[1:2],
      config$cache$exists_object,
      FUN.VALUE = logical(1)
    )
  )
  format <- config$spec[[target]]$format
  if (!is.null(format) && !is.na(format) && !is_external_format(format)) {
    format_file <- config$cache$file_return_key(target)
    out <- out && file.exists(format_file)
  }
  out
}

is_external_format <- function(format) {
  format %in% c(
    "file"
  )
}

is_good_outfile_checksum <- function(target, value, checksum, config) {
  if (!length(checksum)) {
    warn_no_checksum(target = target, config = config)
    return(TRUE)
  }
  if (identical("failed", config$cache$get_progress(target))) {
    return(TRUE) # covered with parallel processes # nocov
  }
  identical(checksum, get_outfile_checksum(target, value, config))
}

get_checksum <- function(target, value, config) {
  paste(
    config$cache$safe_get_hash(
      key = target,
      namespace = config$cache$default_namespace
    ),
    config$cache$safe_get_hash(key = target, namespace = "meta"),
    get_outfile_checksum(target, value, config),
    sep = " "
  )
}

get_outfile_checksum <- function(target, value, config) {
  deps <- config$spec[[target]]$deps_build
  files <- sort(unique(as.character(deps$file_out)))
  out <- vapply(
    X = files,
    FUN = rehash_static_storage,
    FUN.VALUE = character(1),
    config = config
  )
  out <- c(out, format_file_checksum(target, value, config))
  out <- paste(out, collapse = "")
  config$cache$digest(out, serialize = FALSE)
}

format_file_checksum <- function(target, value, config) {
  class(target) <- config$spec[[target]]$format
  format_file_checksum_impl(target, value, config)
}

format_file_checksum_impl <- function(target, value, config) {
  UseMethod("format_file_checksum_impl")
}

#' @export
format_file_checksum_impl.default <- function(target, value, config) { # nolint
  force(value)
  character(0)
}

#' @export
format_file_checksum_impl.file <- function(target, value, config) { # nolint
  hash <- rep(NA_character_, length(value))
  index <- file.exists(value)
  hash[index] <- rehash_local(value[index], config)
  hash
}

warn_no_checksum <- function(target, config) {
  msg <- paste0("No checksum available for target ", target, ".")
  config$logger$disk(paste("Warning:", msg))
  warn0(msg)
}

# Simple version of purrr::pmap for use in drake
# Applies function .f to list .l elements in parallel, i.e.
# .f(.l[[1]][1], .l[[2]][1], ..., .l[[n]][1]) and then
# .f(.l[[1]][2], .l[[2]][2], ..., .l[[n]][2]) etc.
drake_pmap <- function(.l, .f, jobs = 1, ...) {
  stopifnot(is.list(.l))
  stopifnot(is.function(.f))
  stopifnot(is.numeric(jobs))
  if (length(.l) == 0) {
    return(list())  # empty input
  }
  # Ensure identically-lengthed sublists in .l
  len <- unique(unlist(lapply(.l, length)))
  stopifnot(length(len) == 1)
  lightly_parallelize(
    X = seq_len(len),
    FUN = function(i) {
      # extract ith element in each sublist, and then pass to .f
      listi <- lapply(.l, function(x) x[[i]])
      do.call(.f, args = c(listi, ...), quote = TRUE)
    },
    jobs = jobs
  )
}

parallel_filter <- function(x, f, jobs = 1, ...) {
  index <- lightly_parallelize(X = x, FUN = f, jobs = jobs, ...)
  index <- unlist(index)
  x[as.logical(index)]
}

lightly_parallelize <- function(X, FUN, jobs = 1, ...) {
  jobs <- safe_jobs(jobs)
  if (is.atomic(X)) {
    lightly_parallelize_atomic(X = X, FUN = FUN, jobs = jobs, ...)
  } else {
    weak_mclapply(X = X, FUN = FUN, mc.cores = jobs, ...)
  }
}

lightly_parallelize_atomic <- function(X, FUN, jobs = 1, ...) {
  jobs <- safe_jobs(jobs)
  keys <- unique(X)
  index <- match(X, keys)
  values <- weak_mclapply(X = keys, FUN = FUN, mc.cores = jobs, ...)
  values[index]
}

# Avoid SIGCHLD handler when mc.cores is 1.
# Could help avoid zeromq interrupted system call errors.
weak_mclapply <- function(X, FUN, mc.cores, ...) {
  if (mc.cores > 1) {
    mclapply(X = X, FUN = FUN, mc.cores = mc.cores, ...)
  } else {
    lapply(X = X, FUN = FUN, ...)
  }
}

safe_jobs <- function(jobs) {
  ifelse(on_windows(), 1, jobs[1])
}

on_windows <- function() {
  .pkg_envir[["on_windows"]]
}

this_os <- function() {
  unname(tolower(Sys.info()["sysname"]))
}

classify_build <- function(build, config) {
  class <- paste0("drake_build_", config$spec[[build$target]]$format)
  class(build) <- class
  build
}

serialize_build <- function(build) {
  UseMethod("serialize_build")
}

# Requires Python Keras and TensorFlow to test. Tested in test-keras.R.
# nocov start
#' @export
serialize_build.drake_build_keras <- function(build) { # nolint
  assert_pkg("keras")
  build$value <- keras::serialize_model(build$value)
  build
}
# nocov end

#' @export
serialize_build.default <- function(build) {
  build
}

unserialize_build <- function(build) {
  UseMethod("unserialize_build")
}

# Requires Python Keras and TensorFlow to test. Tested in test-keras.R.
# nocov start
#' @export
unserialize_build.drake_build_keras <- function(build) { # nolint
  assert_pkg("keras")
  build$value <- keras::unserialize_model(build$value)
  build
}
# nocov end

#' @export
unserialize_build.default <- function(build) {
  build
}

hpc_worker_build_value <- function(target, value, config) {
  format <- config$spec[[target]]$format %||NA% "none"
  if (format == "file") {
    return(value)
  }
  NULL
}
