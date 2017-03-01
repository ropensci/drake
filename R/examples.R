#' @title Function \code{example_drake}
#' @description Copies the files for a drake example 
#' to your working directory (or \code{out}). Afterwards, you can
#' step through the code in the example to start learning drake.
#' To see the names of all the examples, run \code{\link{examples_drake}}.
#' @seealso \code{\link{examples_drake}}, \code{\link{run}}
#' @export
#' @param example name of the example. To see all the available example names, 
#' run \code{\link{examples_drake}}.
#' @param out name of the folder to put the example
example_drake = function(example = examples_drake(), out = getwd()){
  example = match.arg(example)
  dir = system.file(file.path("examples", example), 
    package = "drake", mustWork = TRUE)
  if(file.exists(example)) 
    stop("There is already a file or folder named ", example, ".", sep = "")
  file.copy(from = dir, to = out, recursive = TRUE)
  invisible()
}

#' @title Function \code{examples_drake}
#' @description List the names of all the drake examples.
#' @seealso \code{\link{example_drake}}, \code{\link{run}}
#' @export
#' @return names of all the drake examples.
examples_drake = function(){
  list.dirs(system.file("examples", package = "drake", mustWork = TRUE), 
    full.names = FALSE, recursive = FALSE)
}
