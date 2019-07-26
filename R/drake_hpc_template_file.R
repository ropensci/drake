
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
