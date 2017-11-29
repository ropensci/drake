#' @title Function \code{diagnose}
#' @description Get the last stored error of a target
#' that failed to build. This target could be a
#' completely failed target or a target
#' that failed initially, retried, then succeeded.
#' If no target is given, then \code{diagnose()} simply
#' lists the targets for which a error is retrievable.
#' Together, functions \code{\link{failed}()} and
#' \code{diagnose()} should eliminate the strict need
#' for ordinary error messages printed to the console.
#' @seealso
#' \code{\link{failed}}, \code{\link{progress}},
#' \code{\link{readd}}, \code{\link{plan_drake}}, \code{\link{make}}
#' @export
#' @return Either a character vector of target names or an object
#' of class \code{"error"}.
#'
#' @param target name of the target of the error to get.
#' Can be a symbol if \code{character_only} is \code{FALSE},
#' must be a character if \code{character_only} is \code{TRUE}.
#'
#' @param character_only logical, whether \code{target} should be treated
#' as a character or a symbol.
#' Just like \code{character.only} in \code{library()}.
#'
#' @param cache optional drake cache. See code{\link{new_cache}()}.
#' If \code{cache} is supplied,
#' the \code{path} and \code{search} arguments are ignored.
#'
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#'
#' @param search If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#'
#' @param verbose whether to print console messages
#'
#' @examples
#' \dontrun{
#' diagnose() # List all the targets with recorded error logs.
#' # Define a function doomed to failure.
#' f <- function(){
#'   stop("unusual error")
#' }
#' # Create a workflow plan doomed to failure.
#' bad_plan <- plan_drake(my_target = f())
#' # Running the project should generate an error
#' # when trying to build 'my_target'.
#' make(bad_plan)
#' failed() # List the failed targets from the last make() (my_target).
#' # List targets that failed at one point or another
#' # over the course of the project (my_target).
#' # Drake keeps all the error logs.
#' diagnose()
#' # Get the error log, an object of class "error".
#' error <- diagnose(my_target)
#' str(error) # See what's inside the error log.
#' error$calls # View the traceback. (See the traceback() function).
#' }
diagnose <- function(
  target = NULL,
  character_only = FALSE,
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = TRUE
){
  if (is.null(cache)){
    return(character(0))
  }
  if (!character_only){
    target <- as.character(substitute(target))
  }
  targets <- cache$list(namespace = "errors") %>%
    sort
  if (!length(targets)){
    return(character(0))
  }
  if (!length(target)){
    return(targets)
  }
  if (!(target %in% targets)){
    stop("No diagnostic information for target ", target, ".")
  }
  cache$get(key = target, namespace = "errors")
}
