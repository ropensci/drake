#' @title Function \code{workflow}
#' @description Turns a named collection of \code{remake} commands into 
#' a data frame of \code{remake} targets and \code{remake} workflow.
#' \code{\link{workflow_string}} and \code{\link{workflow_batch}} are similar.
#' @seealso \code{\link{workflow_string}}, \code{\link{workflow_batch}}, 
#' @export
#' @return data frame of remake targets and commands
#' @param ... workflow named with their respective targets
workflow = function(...) {
  workflow_batch(structure(as.list(match.call()[-1]), class = "uneval"))
}

#' @title Function \code{workflow_string}
#' @description Similar to \code{\link{workflow}} except that commands are
#' parsed as strings rather than symbols. \code{\link{workflow_batch}} is
#' another alternative.
#' @seealso \code{\link{workflow}}, \code{\link{workflow_batch}},
#' @export
#' @return data frame of skipit targets and commands
#' @param ... workflow named with their respective targets
workflow_string = function(...) {
  workflow_batch(list(...))
}

#' @title Function \code{workflow_batch}
#' @description Similar to \code{\link{workflow}} except that commands are
#' given as a named character vector with targets as names and commands as elements.
#' \code{\link{workflow_string}} is another alternative.
#' @seealso \code{\link{workflow}}, \code{\link{workflow_string}},
#' @export
#' @return data frame of remake targets and workflow
#' @param x named character vector with targets as names and workflow as elements
workflow_batch = function(x = NULL) {
  if(!length(x)) return(data.frame(target = character(0), command = character(0)))
  out = data.frame(target = names(x), command = as.character(x), stringsAsFactors = FALSE)
  rownames(out) = NULL
  assert_workflow(out)
  out
}

assert_workflow = function(x){
  assert_that(is.data.frame(x))
  if(is.null(x$target) | any(!nchar(x$target)) | any(!nchar(x$command))) 
    stop("All commands and their targets must be given. For example, write workflow(x = data(y), z = 3) instead of workflow(x, z) or workflow(data(y), 3).")
  if(anyDuplicated(x$target)) stop("commands must be given unique target names. No duplicate names allowed.")
}
