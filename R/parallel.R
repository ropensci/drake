run_parallel_backend <- function(config) {
  get(
    paste0("run_", config$parallelism),
    envir = getNamespace("drake")
  )(config)
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
    safe_mclapply(X = X, FUN = FUN, mc.cores = jobs, ...)
  }
}

lightly_parallelize_atomic <- function(X, FUN, jobs = 1, ...) {
  jobs <- safe_jobs(jobs)
  keys <- unique(X)
  index <- match(X, keys)
  values <- safe_mclapply(X = keys, FUN = FUN, mc.cores = jobs, ...)
  values[index]
}

# Avoid SIGCHLD handler when mc.cores is 1.
# Could help avoid zeromq interrupted system call errors.
safe_mclapply <- function(X, FUN, mc.cores, ...) {
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
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # List the available template files.
#' drake_hpc_template_files()
#' # Write a SLURM template file from the SLURM example.
#' drake_hpc_template_file("slurm_batchtools.tmpl") # Writes slurm_batchtools.tmpl.
#' # library(future.batchtools) # nolint
#' # future::plan(batchtools_slurm, template = "slurm_batchtools.tmpl") # nolint
#' # make(my_plan, parallelism = "future", jobs = 2) # nolint
#' })
#' }
drake_hpc_template_file <- function(
  file = drake::drake_hpc_template_files(),
  to = getwd(),
  overwrite = FALSE
) {
  file <- match.arg(file)
  dir <- system.file(
    "hpc_template_files",
    package = "drake",
    mustWork = TRUE
  )
  file.copy(from = file.path(dir, file), to = to,
            overwrite = overwrite, recursive = TRUE)
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
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # List the available template files.
#' drake_hpc_template_files()
#' # Write a SLURM template file from the SLURM example.
#' drake_hpc_template_file("slurm_batchtools.tmpl") # Writes slurm_batchtools.tmpl.
#' # library(future.batchtools) # nolint
#' # future::plan(batchtools_slurm, template = "slurm_batchtools.tmpl") # nolint
#' # make(my_plan, parallelism = "future", jobs = 2) # nolint
#' })
#' }
drake_hpc_template_files <- function() {
  dir(
    system.file(
      "hpc_template_files",
      package = "drake",
      mustWork = TRUE
    )
  )
}
