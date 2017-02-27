#' @title Function \code{plan}
#' @description Turns a named collection of command/target pairs into 
#' a workflow plan data frame for \code{\link{make}} and 
#' \code{\link{check}}.
#' @details Drake uses single quotes to denote external files
#' and double-quoted strings as ordinary strings. 
#' Quotes in the \code{list} argument are left alone,
#' but R messes with quotes when it parses the freeform 
#' arguments in \code{...}, so use the \code{strings_in_dots}
#' argument to control the quoting in \code{...}.
#' @seealso \code{link{check}}, \code{\link{make}}, 
#' @export
#' @return data frame of targets and command
#' @param ... pieces of command named according to their respective targets.
#' Recall that drake uses single quotes to denote external files
#' and double-quoted strings as ordinary strings.
#' Use the \code{strings_in_dots} argument to control the
#' quoting in \code{...}.
#' @param list named character vector of pieces of command named
#' according to their respective targets
#' @param file_targets logical. If \code{TRUE}, targets are single-quoted
#' to tell drake that these are external files that should be generated
#' in the next call to \code{\link{make}()}.
#' @param strings_in_dots character scalar. If \code{"filenames"},
#' all character strings in \code{...} will be treated as names of file
#' dependencies (single-quoted). If \code{"literals"}, all
#' character strings in \code{...} will be treated as ordinary
#' strings, not dependencies of any sort (double-quoted). 
#' (This does not affect the names of free-form arguments passed to 
#' \code{...}). Because of R's
#' automatic parsing/deparsing behavior, strings in \code{...}
#' cannot simply be left alone.
plan = function(..., list = character(0), file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")) {
  strings_in_dots = match.arg(strings_in_dots)
  dots = match.call(expand.dots = FALSE)$...
  target = lapply(dots, deparse)
  names(target) = names(dots)
  x = c(target, list)
  if(!length(x)) return(data.frame(target = character(0),
    command = character(0)))
  out = data.frame(target = names(x), command = as.character(x),
    stringsAsFactors = FALSE)
  i = out$target %in% names(target)
  if(file_targets) out$target = quotes(out$target, single = T)
  if(strings_in_dots == "file_deps") 
    out$command[i] = gsub("\"", "'", out$command[i])
  out
}
