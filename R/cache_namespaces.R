#' @title List all the `storr` cache namespaces used by drake.
#' @export
#' @seealso [make()]
#' @return A character vector of `storr` namespaces used for drake.
#' @description Ordinary users do not need to worry about this function.
#' It is just another window into `drake`'s internals.
#' @param default name of the default `storr` namespace
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
#'   list the `storr` namespaces that are cleaned
#'   during a call to [clean()].
#' @description All these
#' namespaces store target-level data, but not all
#' target-level namespaces are cleaned during
#' [clean()].
#' @export
#' @seealso [cache_namespaces()], [clean()]
#' @return A character vector of `storr` namespaces
#'   that are cleaned during [clean()].
#' @param default Name of the default `storr` namespace.
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
#'   list the `storr` cache namespaces
#'   that store target-level information.
#' @export
#' @seealso [make()]
#' @return A character vector of `storr` namespaces that store
#'   target-level information.
#' @description Ordinary users do not need to worry about this function.
#' It is just another window into `drake`'s internals.
#' @param default name of the default `storr` namespace
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
