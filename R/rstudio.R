#' @title RStudio addin for r_make()
#' \lifecycle{stable}
#' @description Call [r_make()] in an RStudio addin.
#' @return Nothing.
#' @inheritParams r_make
#' @keywords internal
#' @export
rs_addin_r_make <- function(r_args = list()) {
  r_make(r_args = r_args)
}

#' @title RStudio addin for r_outdated()
#' \lifecycle{stable}
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
#' \lifecycle{stable}
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

#' @title Loadd target at cursor into global environment
#' \lifecycle{stable}
#' @description This function provides an RStudio addin that will
#' load the target at the
#' current cursor location from the cache into the global environment.
#' This is convenient during pipeline development when building off
#' established targets.
#' @param context an RStudio document context.
#'   Read from the active document if not supplied.
#'   This is used for testing purposes.
#' @return Nothing.
#' @keywords internal
#' @export
rs_addin_loadd <- function(context = NULL){
  assert_pkg("rstudioapi")
  context <- context %||% rstudioapi::getActiveDocumentContext()
  target <- rs_get_symbol_at_cursor(context)
  if (is.null(target)) {
    return()
  }
  cache <- drake_cache()
  message(
    "Loading target ",
    shQuote(target),
    " into global evironment.\nCache: ",
    shQuote(cache$path)
  )
  loadd(
    list = target,
    envir = globalenv(),
    cache = cache
  )
}

rs_get_symbol_at_cursor <- function(context) {
  if (identical(context$id, "#console")) {
    return(NULL)
  }
  cursor_pos <- context$selection[[1]]$range$start
  cursor_line <- cursor_pos[1]
  cursor_column <- cursor_pos[2]
  r_symbol_pattern <- "[.A-Za-z][.A-Za-z0-9_]+"
  line_symbols <- gregexpr(
    text = context$contents[cursor_line],
    pattern = r_symbol_pattern
  )
  match_starts <- line_symbols[[1]]
  match_ends <- match_starts + attr(x = line_symbols[[1]], "match.length") - 1
  match_index <- which(
    cursor_column >= match_starts &
      cursor_column <= match_ends
  )
  if (length(match_index) == 0){
    message("Couldn't find an object name at cursor position.")
    return(NULL)
  }
  substr(
    context$contents[cursor_line],
    start = match_starts[match_index],
    stop = match_ends[match_index]
  )
}
