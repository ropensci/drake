#' @title Write a template file for deploying
#'   work to a cluster / job scheduler.
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

wait_outfile_checksum <- function(target, checksum, config, timeout = 300) {
  wait_checksum(
    target = target,
    checksum = checksum,
    config = config,
    timeout = timeout,
    criterion = is_good_outfile_checksum
  )
}

wait_checksum <- function(
  target,
  checksum,
  config,
  timeout = 300,
  criterion = is_good_checksum
) {
  i <- 0
  time_left <- timeout
  while (time_left > 0) {
    if (criterion(target, checksum, config)) {
      return()
    } else {
      sleep <- config$sleep(max(0L, i))
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
  drake_log(paste("Error:", msg), config = config)
  stop(msg, call. = FALSE)
}

is_good_checksum <- function(target, checksum, config) {
  # Not actually reached, but may come in handy later.
  # nocov start
  if (!length(checksum)) {
    warn_no_checksum(target = target, config = config)
    return(TRUE)
  }
  if (identical("failed", get_progress_single(target, cache = config$cache))) {
    return(TRUE) # covered with parallel processes # nocov
  }
  # nocov end
  local_checksum <- get_checksum(target = target, config = config)
  if (!identical(local_checksum, checksum)) {
    return(FALSE)
  }
  all(
    vapply(
      X = unlist(strsplit(local_checksum, " "))[1:2],
      config$cache$exists_object,
      FUN.VALUE = logical(1)
    )
  )
}

is_good_outfile_checksum <- function(target, checksum, config) {
  if (!length(checksum)) {
    warn_no_checksum(target = target, config = config)
    return(TRUE)
  }
  if (identical("failed", get_progress_single(target, cache = config$cache))) {
    return(TRUE) # covered with parallel processes # nocov
  }
  identical(checksum, get_outfile_checksum(target = target, config = config))
}

get_checksum <- function(target, config) {
  paste(
    safe_get_hash(
      key = target,
      namespace = config$cache$default_namespace,
      config = config
    ),
    safe_get_hash(key = target, namespace = "meta", config = config),
    get_outfile_checksum(target, config),
    sep = " "
  )
}

get_outfile_checksum <- function(target, config) {
  deps <- config$layout[[target]]$deps_build
  files <- sort(unique(as.character(deps$file_out)))
  out <- vapply(
    X = files,
    FUN = rehash_storage,
    FUN.VALUE = character(1),
    config = config
  )
  out <- paste(out, collapse = "")
  digest::digest(
    out,
    algo = config$hash_algorithm,
    serialize = FALSE
  )
}

warn_no_checksum <- function(target, config) {
  msg <- paste0("No checksum available for target ", target, ".")
  drake_log(paste("Warning:", msg), config = config)
  warning(msg, call. = FALSE)
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
    jobs = jobs)
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
    parallel::mclapply(X = X, FUN = FUN, mc.cores = mc.cores, ...)
  } else {
    lapply(X = X, FUN = FUN, ...)
  }
}

safe_jobs <- function(jobs) {
  stopifnot(length(jobs) == 1)
  ifelse(on_windows(), 1, jobs)
}

on_windows <- function() {
  this_os() == "windows"
}

this_os <- function() {
  unname(tolower(Sys.info()["sysname"]))
}