#' @title Function \command{plan}
#' @description Turns a named collection of command/target pairs into 
#' a plan plan data frame for \command{\link{run}} and 
#' \command{\link{check}}.
#' @details Drake uses single quotes to denote external files
#' and double-quoted strings as ordinary strings. 
#' Quotes in the \command{list} argument are left alone,
#' but R messes with quotes when it parses the freeform 
#' arguments in \command{...}, so use the \command{strings_in_dots}
#' argument to control the quoting in \command{...}.
#' @seealso \command{link{check}}, \command{\link{run}}, 
#' @export
#' @return data frame of targets and command
#' @param ... pieces of command named according to their respective targets.
#' Recall that drake uses single quotes to denote external files
#' and double-quoted strings as ordinary strings.
#' Use the \command{strings_in_dots} argument to control the
#' quoting in \command{...}.
#' @param list named character vector of pieces of command named
#' according to their respective targets
#' @param file_targets logical. If \command{TRUE}, targets are single-quoted
#' to tell drake that these are external files that should be generated
#' in the next call to \command{\link{run}()}.
#' @param strings_in_dots character scalar. If \command{"filenames"},
#' all character strings in \command{...} will be treated as names of file
#' dependencies (single-quoted). If \command{"literals"}, all
#' character strings in \command{...} will be treated as ordinary
#' strings, not dependencies of any sort (double-quoted). 
#' (This does not affect the names of free-form arguments passed to 
#' \command{...}). Because of R's
#' automatic parsing/deparsing behavior, strings in \command{...}
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
