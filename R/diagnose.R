#' @title Get diagnostic metadata on a target.
#' @description Diagnostics include errors, warnings,
#'   messages, runtimes, and other context from when a
#'   target was built or an import was processed.
#'   If your target's last build succeeded,
#'   then `diagnose(your_target)` has the most current information
#'   from that build.
#'   But if your target failed, then only
#'   `diagnose(your_target)$error`,
#'   `diagnose(your_target)$warnings`,
#'   and `diagnose(your_target)$messages` correspond to the failure,
#'   and all the other metadata correspond to the last build that completed
#'   without an error.
#' @seealso
#'   [failed()], [progress()],
#'   [readd()], [drake_plan()], [make()]
#' @export
#' @return Either a character vector of target names or an object
#'   of class `"error"`.
#'
#' @inheritParams cached
#'
#' @param target name of the target of the error to get.
#'   Can be a symbol if `character_only` is `FALSE`,
#'   must be a character if `character_only` is `TRUE`.
#'
#' @param character_only logical, whether `target` should be treated
#'   as a character or a symbol.
#'   Just like `character.only` in [library()].
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
#' error <- diagnose(my_target)$error # See also warnings and messages.
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
    return(cache$list(namespace = "meta"))
  }
  if (!cache$exists(key = target, namespace = "meta")){
    stop("No diagnostic information for target ", target, ".")
  }
  cache$get(
    key = standardize_filename(target),
    namespace = "meta",
    use_cache = FALSE
  )
}
