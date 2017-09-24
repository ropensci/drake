#' @title Function \code{example_drake}
#' @description Copy a folder of code files for a
#' drake example to the current working directory.
#' Call \code{example_drake('basic')} to generate the code files from the
#' quickstart vignette: \code{vignette('quickstart')}.
#' To see the names of all the examples, run \code{\link{examples_drake}}.
#' @seealso \code{\link{examples_drake}}, \code{\link{make}}
#' @export
#' @param example name of the example.
#' To see all the available example names,
#' run \code{\link{examples_drake}}.
#' @param destination character scalar, file path, where
#' to write the folder containing the code files for the example.
#' @examples
#' \dontrun{
#' example_drake('basic') # Same as the quickstart vignette
#' }
example_drake <- function(example = drake::examples_drake(),
  destination = getwd()) {
  example <- match.arg(example)
  dir <- system.file(file.path("examples", example), package = "drake",
    mustWork = TRUE)
  if (file.exists(example))
    stop("There is already a file or folder named ", example,
      ".", sep = "")
  file.copy(from = dir, to = destination, recursive = TRUE)
  invisible()
}

#' @title Function \code{examples_drake}
#' @description List the names of all the drake examples.
#' The \code{'basic'} example is the one from the
#' quickstart vignette: \code{vignette('quickstart')}.
#' @export
#' @seealso \code{\link{example_drake}}, \code{\link{make}}
#' @return names of all the drake examples.
#' @examples
#' examples_drake()
examples_drake <- function() {
  list.dirs(system.file("examples", package = "drake", mustWork = TRUE),
    full.names = FALSE, recursive = FALSE)
}
