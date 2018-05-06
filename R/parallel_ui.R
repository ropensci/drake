#' @title Write the batchtools template file
#' from one of the built-in drake examples.
#' @description If there are multiple template files in the example,
#' only the first one (alphabetically) is written.
#' @export
#' @seealso [drake_examples()], [drake_example()],
#'   [shell_file()]
#' @return `NULL` is returned,
#'   but a batchtools template file is written.
#' @param example Name of the drake example
#'   from which to take the template file.
#'   Must be listed in [drake_examples()].
#' @param to Character vector, where to write the file.
#' @param overwrite Logical, whether to overwrite an existing file of the
#'   same name.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # List the drake examples. Only some have template files.
#' drake_examples()
#' # Write the batchtools template file from the SLURM example.
#' drake_batchtools_tmpl_file("slurm") # Writes batchtools.slurm.tmpl.
#' # Find batchtools.slurm.tmpl with the rest of the example's files.
#' drake_example("slurm") # Writes a new 'slurm' folder with more files.
#' # Run the mtcars example with a
#' # SLURM-powered parallel backend. Requires SLURM.
#' library(future.batchtools)
#' # future::plan(batchtools_slurm, template = "batchtools.slurm.tmpl") # nolint
#' # make(my_plan, parallelism = "future_lapply") # nolint
#' })
#' }
drake_batchtools_tmpl_file <- function(
  example = drake::drake_examples(),
  to = getwd(),
  overwrite = FALSE
){
  example <- match.arg(example)
  dir <- system.file(file.path("examples", example), package = "drake",
    mustWork = TRUE)
  files <- list.files(dir)
  template_files <- files[grepl("\\.tmpl$", files)]
  if (!length(template_files)){
    stop("No template files found for the ", example, " example.")
  }
  file <- file.path(dir, template_files[1])
  file.copy(from = file, to = to,
    overwrite = overwrite, recursive = TRUE)
  invisible()
}

#' @title List the types of supported parallel computing in drake.
#' @description These are the possible values of the
#' `parallelism` argument to [make()].
#' @export
#' @seealso [make()], [shell_file()]
#' @return Character vector listing the types of parallel
#'   computing supported.
#'
#' @details See the
#'   [parallel computing guide](https://ropensci.github.io/drake/articles/parallelism.html) # nolint
#'   for details on the parallel backends.
#'
#' @param distributed_only logical, whether to return only
#'   the distributed backend types, such as `Makefile` and
#'   `parLapply`
#'
#' @examples
#' # See all the parallel computing options.
#' parallelism_choices()
#' # See just the distributed computing options.
#' parallelism_choices(distributed_only = TRUE)
parallelism_choices <- function(distributed_only = FALSE) {
  local <- c(
    "mclapply",
    "parLapply",
    "mclapply_staged",
    "parLapply_staged"
  )
  distributed <- c(
    "future",
    "future_lapply",
    "Makefile"
  )
  if (distributed_only){
    distributed
  } else {
    c(local, distributed)
  }
}

#' @title Show the default `parallelism` argument
#'   to [make()] for your system.
#' @description Returns `'parLapply'` for Windows machines
#' and `'mclapply'` for other platforms.
#' @export
#' @seealso [make()], [shell_file()]
#' @return The default parallelism option for your system.
#' @examples
#' default_parallelism()
default_parallelism <- function() {
  ifelse(on_windows(), "parLapply", "mclapply") %>%
    unname
}

#' @title Write an example `shell.sh` file required by
#'   `make(..., parallelism = 'Makefile', prepend = 'SHELL=./shell.sh')`.
#' @description This function also does a `chmod +x`
#' to enable execute permissions.
#' @seealso [make()], [max_useful_jobs()],
#'   [parallelism_choices()], [drake_batchtools_tmpl_file()],
#'   [drake_example()], [drake_examples()]
#' @export
#' @return The return value of the call to [file.copy()] that
#'   wrote the shell file.
#' @param path file path of the shell file
#' @param overwrite logical, whether to overwrite a possible
#'   destination file with the same name
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # Write shell.sh to your working directory.
#' # Read the parallelism vignette to learn how it is used
#' # in Makefile parallelism.
#' shell_file()
#' })
#' }
shell_file <- function(
  path = "shell.sh",
  overwrite = FALSE
){
  from <- system.file("shell.sh", package = "drake", mustWork = TRUE)
  if (file.exists(path) & overwrite){
    warning("Overwriting file ", path)
  }
  invisible(file.copy(from = from, to = path, copy.mode = TRUE,
    overwrite = overwrite))
}
