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
#' \code{\link{readd}}, \code{\link{workflow}}, \code{\link{make}}
#' @export
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
#' @examples
#' \dontrun{
#' diagnose()
#' f <- function(){
#'   stop("unusual error")
#' }
#' bad_plan <- workflow(my_target = f())
#' make(bad_plan)
#' failed() # from the last make() only
#' diagnose() # from all previous make()'s
#' error <- diagnose(my_target)
#' str(error)
#' error$calls # View the traceback.
#' }
diagnose <- function(
  target = NULL,
  character_only = FALSE,
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search)
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
