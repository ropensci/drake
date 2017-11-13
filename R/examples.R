#' @title Function \code{drake_example}
#' @description Copy a folder of code files for a
#' drake example to the current working directory.
#' Call \code{drake_example('basic')} to generate the code files from the
#' quickstart vignette: \code{vignette('quickstart')}.
#' To see the names of all the examples, run \code{\link{drake_examples}}.
#' @seealso \code{\link{drake_examples}}, \code{\link{make}}
#' @export
#' @return \code{NULL}
#' @param example name of the example.
#' To see all the available example names,
#' run \code{\link{drake_examples}}.
#' @param destination character scalar, file path, where
#' to write the folder containing the code files for the example.
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
  destination = getwd()
){
  example <- match.arg(example)
  dir <- system.file(file.path("examples", example), package = "drake",
    mustWork = TRUE)
  if (file.exists(example))
    stop("There is already a file or folder named ", example,
      ".", sep = "")
  file.copy(from = dir, to = destination, recursive = TRUE)
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
