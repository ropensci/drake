#' @title Function cache_namespaces
#' @export
#' @seealso \code{\link{make}}
#' @return A character vector of \code{storr} namespaces used for drake.
#' @description List all the \code{storr} cache namespaces used by drake.
#' Ordinary users do not need to worry about this function.
#' It is just another window into \code{drake}'s internals.
#' @param default name of the default \code{storr} namespace
#' @examples
#' cache_namespaces()
cache_namespaces <- function(
  default = storr::storr_environment()$default_namespace
){
  c(
    target_namespaces(default = default),
    "config",
    "max_useful_jobs",
    "session"
  ) %>%
    sort
}

#' @title Function cleaned_namespaces
#' @description List the \code{storr} namespaces that are cleaned
#' during a call to \code{\link{clean}()}. All these
#' namespaces store target-level data, but not all
#' target-level namespaces are cleaned during
#' \code{\link{clean}()}.
#' @export
#' @seealso \code{\link{cache_namespaces}}, \code{\link{clean}}
#' @return A character vector of \code{storr} namespaces
#' that are cleaned during \code{\link{clean}()}.
#' @param default Name of the default \code{storr} namespace.
#' @examples
#' cleaned_namespaces()
cleaned_namespaces <- function(
  default = storr::storr_environment()$default_namespace
){
  c(
    default,
    "commands",
    "depends",
    "kernels",
    "mtimes"
  ) %>%
    sort
}

#' @title Function target_namespaces
#' @export
#' @seealso \code{\link{make}}
#' @return A character vector of \code{storr} namespaces that store
#' target-level information.
#' @description List the \code{storr} cache namespaces
#' that store target-level information.
#' Ordinary users do not need to worry about this function.
#' It is just another window into \code{drake}'s internals.
#' @param default name of the default \code{storr} namespace
#' @examples
#' target_namespaces()
target_namespaces <- function(
  default = storr::storr_environment()$default_namespace
){
  c(
    cleaned_namespaces(default = default),
    "build_times",
    "errors",
    "meta",
    "progress"
  ) %>%
    sort
}
