#' @title List all the \code{storr} cache namespaces used by drake.
#' @export
#' @seealso \code{\link{make}}
#' @return A character vector of \code{storr} namespaces used for drake.
#' @description Ordinary users do not need to worry about this function.
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
    "session"
  ) %>%
    sort
}

#' @title For drake caches,
#' list the \code{storr} namespaces that are cleaned
#' during a call to \code{\link{clean}()}.
#' @description All these
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
    "kernels",
    "meta"
  ) %>%
    sort
}

#' @title For drake caches,
#' list the \code{storr} cache namespaces
#' that store target-level information.
#' @export
#' @seealso \code{\link{make}}
#' @return A character vector of \code{storr} namespaces that store
#' target-level information.
#' @description Ordinary users do not need to worry about this function.
#' It is just another window into \code{drake}'s internals.
#' @param default name of the default \code{storr} namespace
#' @examples
#' target_namespaces()
target_namespaces <- function(
  default = storr::storr_environment()$default_namespace
){
  c(
    cleaned_namespaces(default = default),
    "errors",
    "progress"
  ) %>%
    sort
}
