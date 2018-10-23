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
#' library(future.batchtools)
#' # future::plan(batchtools_slurm, template = "slurm_batchtools.tmpl") # nolint
#' # make(my_plan, parallelism = "future", jobs = 2) # nolint
#' })
#' }
drake_hpc_template_file <- function(
  file = drake::drake_hpc_template_files(),
  to = getwd(),
  overwrite = FALSE
){
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
#' @return a character vector of example template files that
#'   you can write with [drake_hpc_template_file()].
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # List the available template files.
#' drake_hpc_template_files()
#' # Write a SLURM template file from the SLURM example.
#' drake_hpc_template_file("slurm_batchtools.tmpl") # Writes slurm_batchtools.tmpl.
#' library(future.batchtools)
#' # future::plan(batchtools_slurm, template = "slurm_batchtools.tmpl") # nolint
#' # make(my_plan, parallelism = "future", jobs = 2) # nolint
#' })
#' }
drake_hpc_template_files <- function(){
  dir(
    system.file(
      "hpc_template_files",
      package = "drake",
      mustWork = TRUE
    )
  )
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
#'   [high-performance computing guide](https://ropenscilabs.github.io/drake-manual/hpc.html) # nolint
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
    "clustermq",
    "clustermq_staged",
    "future",
    "future_lapply",
    "future_lapply_staged",
    "hasty",
    "Makefile"
  )
  if (distributed_only){
    sort(distributed)
  } else {
    sort(c(local, distributed))
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
#' @seealso [make()]
#'   [parallelism_choices()], [drake_hpc_template_file()],
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
#' # Read the high-performance computing chapter
#' # (https://ropenscilabs.github.io/drake-manual/hpc.html)
#' # to learn how it is used
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
