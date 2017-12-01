#' @title Function \code{drake_example}
#' @description Copy a folder of code files for a
#' drake example to the current working directory.
#' Call \code{drake_example('basic')} to generate the code files from the
#' quickstart vignette: \code{vignette('quickstart')}.
#' To see the names of all the examples, run \code{\link{drake_examples}}.
#' @seealso \code{\link{drake_examples}}, \code{\link{make}},
#' \code{\link{shell_file}}, \code{\link{drake_batchtools_tmpl_file}}
#' @export
#' @return \code{NULL}
#' @param example name of the example.
#' To see all the available example names,
#' run \code{\link{drake_examples}}.
#' @param to Character scalar, file path, where
#' to write the folder containing the code files for the example.
#' @param destination Deprecated, use \code{to} instead.
#' @param overwrite Logical, whether to overwrite an existing folder
#' with the same name as the drake example.
#' @examples
#' \dontrun{
#' drake_examples() # List all the drake examples.
#' # Sets up the same example as the quickstart vignette.
#' drake_example("basic")
#' # Sets up the SLURM example.
#' drake_example("slurm")
#' }
drake_example <- function(
  example = drake::drake_examples(),
  to = getwd(),
  destination = NULL,
  overwrite = FALSE
){
  if (!is.null(destination)){
    warning(
      "The 'destination' argument of drake_example() is deprecated. ",
      "Use 'to' instead."
    )
    to <- destination
  }
  example <- match.arg(example)
  dir <- system.file(file.path("examples", example), package = "drake",
    mustWork = TRUE)
  file.copy(from = dir, to = to,
    overwrite = overwrite, recursive = TRUE)
  invisible()
}

#' @title Function \code{drake_examples}
#' @description List the names of all the drake examples.
#' The \code{'basic'} example is the one from the
#' quickstart vignette: \code{vignette('quickstart')}.
#' @export
#' @seealso \code{\link{drake_example}}, \code{\link{make}}
#' @return Names of all the drake examples.
#' @examples
#' \dontrun{
#' drake_examples() # List all the drake examples.
#' # Sets up the same example as the quickstart vignette.
#' drake_example("basic")
#' # Sets up the SLURM example.
#' drake_example("slurm")
#' }
drake_examples <- function() {
  list.dirs(system.file("examples", package = "drake", mustWork = TRUE),
    full.names = FALSE, recursive = FALSE)
}
