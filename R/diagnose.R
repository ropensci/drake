#' @title Get the last stored error log of a target
#' that failed to build, or list the targets with error logs.
#' @description The specified target could be a
#' completely failed target or a target
#' that failed initially, retried, then succeeded.
#' If no target is given, then `diagnose()` simply
#' lists the targets for which a error is retrievable.
#' Together, functions [failed()] and
#' `diagnose()` should eliminate the strict need
#' for ordinary error messages printed to the console.
#' @seealso
#' [failed()], [progress()],
#' [readd()], [drake_plan()], [make()]
#' @export
#' @return Either a character vector of target names or an object
#' of class `"error"`.
#'
#' @param target name of the target of the error to get.
#' Can be a symbol if `character_only` is `FALSE`,
#' must be a character if `character_only` is `TRUE`.
#'
#' @param character_only logical, whether `target` should be treated
#' as a character or a symbol.
#' Just like `character.only` in [library()].
#'
#' @param cache optional drake cache. See [new_cache()].
#' If `cache` is supplied,
#' the `path` and `search` arguments are ignored.
#'
#' @param path Root directory of the drake project,
#' or if `search` is `TRUE`, either the
#' project root or a subdirectory of the project.
#'
#' @param search If `TRUE`, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#'
#' @param verbose whether to print console messages
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' diagnose() # List all the targets with recorded error logs.
#' # Define a function doomed to failure.
#' f <- function(){
#'   stop("unusual error")
#' }
#' # Create a workflow plan doomed to failure.
#' bad_plan <- drake_plan(my_target = f())
#' # Running the project should generate an error
#' # when trying to build 'my_target'.
#' try(make(bad_plan), silent = FALSE)
#' failed() # List the failed targets from the last make() (my_target).
#' # List targets that failed at one point or another
#' # over the course of the project (my_target).
#' # Drake keeps all the error logs.
#' diagnose()
#' # Get the error log, an object of class "error".
#' error <- diagnose(my_target)
#' str(error) # See what's inside the error log.
#' error$calls # View the traceback. (See the traceback() function).
#' })
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
  if (!length(target)){
    return(cache$list(namespace = "errors"))
  }
  if (!cache$exists(key = target, namespace = "errors")){
    stop("No diagnostic information for target ", target, ".")
  }
  cache$get(key = target, namespace = "errors")
}
