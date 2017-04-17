#' @title Function \code{plan}
#' @description Turns a named collection of command/target pairs into 
#' a workflow plan data frame for \code{\link{make}} and 
#' \code{\link{check}}.
#' @details A workflow plan data frame is a data frame
#' with a \code{target} column and a \code{command} column.
#' Targets are the objects and files that drake generates,
#' and commands are the pieces of R code that produce them.
#' 
#' For file inputs and targets, drake uses single quotes.
#' Double quotes are reserved for ordinary strings. 
#' The distinction is important because drake thinks about
#' how files, objects, targets, etc. depend on each other.
#' Quotes in the \code{list} argument are left alone,
#' but R messes with quotes when it parses the freeform 
#' arguments in \code{...}, so use the \code{strings_in_dots}
#' argument to control the quoting in \code{...}.
#' @seealso \code{link{check}}, \code{\link{make}}, 
#' @export
#' @return data frame of targets and command
#' @param ... commands named by the targets they generate.
#' Recall that drake uses single quotes to denote external files
#' and double-quoted strings as ordinary strings.
#' Use the \code{strings_in_dots} argument to control the
#' quoting in \code{...}.
#' @param list character vector of commands named
#' by the targets they generate.
#' @param file_targets logical. If \code{TRUE}, targets are single-quoted
#' to tell drake that these are external files that should be expected 
#' as output in the next call to \code{\link{make}()}.
#' @param strings_in_dots character scalar. If \code{"filenames"},
#' all character strings in \code{...} will be treated as names of file
#' dependencies (single-quoted). If \code{"literals"}, all
#' character strings in \code{...} will be treated as ordinary
#' strings, not dependencies of any sort (double-quoted). 
#' Because of R's automatic parsing/deparsing behavior, 
#' strings in \code{...} cannot simply be left alone.
plan = function(..., list = character(0), file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")) {
  strings_in_dots = match.arg(strings_in_dots)
  dots = match.call(expand.dots = FALSE)$...
  commands_dots = lapply(dots, wide_deparse)
  names(commands_dots) = names(dots)
  commands = c(commands_dots, list)
  targets = names(commands)
  commands = as.character(commands)
  if(!length(commands)) return(data.frame(target = character(0),
    command = character(0)))
  plan = data.frame(target = targets, command = commands, 
    stringsAsFactors = FALSE)
  from_dots = plan$target %in% names(commands_dots)
  if(file_targets) plan$target = quotes(plan$target, single = T)
  if(strings_in_dots == "filenames") 
    plan$command[from_dots] = gsub("\"", "'", plan$command[from_dots])
  sanitize_plan(plan)
}

#' @title Function \code{as_file}
#' @description Converts an ordinary character string
#' into a filename understandable by drake. In other words,
#' \code{as_file(x)} just wraps single quotes around \code{x}.
#' @export
#' @return a single-quoted character string: i.e., a filename
#' understandable by drake.
#' @param x character string to be turned into a filename
#' understandable by drake (i.e., a string with literal
#' single quotes on both ends).
as_file = function(x){
  quotes(x, single = TRUE)
}

wide_deparse = function(x){
  paste(deparse(x), collapse = "")
}

sanitize_plan = function(plan){
  for(field in c("target", "output"))
    if(!is.null(plan[[field]])) plan[[field]] = 
      str_trim(plan[[field]], side = "both")
  as.data.frame(plan, stringsAsFactors = FALSE) %>%
    fix_deprecated_plan_names
}
