#' @title RStudio addin for r_make()
#' @description Call [r_make()] in an RStudio addin.
#' @return Nothing.
#' @inheritParams r_make
#' @keywords internal
#' @export
rs_addin_r_make <- function(r_args = list()) {
  r_make(r_args = r_args)
}

#' @title RStudio addin for r_outdated()
#' @description Call [r_outdated()] in an RStudio addin.
#' @return A character vector of outdated targets.
#' @inheritParams r_make
#' @param .print Logical, whether to `print()` the result
#'   to the console. Required for the addin.
#' @keywords internal
#' @export
rs_addin_r_outdated <- function(r_args = list(), .print = TRUE) {
  out <- r_outdated(r_args = r_args)
  if (.print) {
    print(out) # nocov
  } else {
    out
  }
}

#' @title RStudio addin for r_vis_drake_graph()
#' @description Call [r_vis_drake_graph()] in an RStudio addin.
#' @return A `visNetwork` graph.
#' @inheritParams r_make
#' @inheritParams rs_addin_r_outdated
#' @keywords internal
#' @export
rs_addin_r_vis_drake_graph <- function(r_args = list(), .print = TRUE) {
  assert_pkg("visNetwork")
  requireNamespace("visNetwork")
  out <- r_vis_drake_graph(r_args = r_args)
  if (.print) {
    print(out) # nocov
  } else {
    out
  }
}
