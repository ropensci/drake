#' @title Function \code{workplan}
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
#' but R messes with quotes when it parses the free-form
#' arguments in \code{...}, so use the \code{strings_in_dots}
#' argument to control the quoting in \code{...}.
#' @export
#' @return A data frame of targets and commands.
#' @param ... same as for \code{drake::\link{workplan}()}
#' @param list same as for \code{drake::\link{workplan}()}
#' @param file_targets same as for \code{drake::\link{workplan}()}
#' @param strings_in_dots same as for \code{drake::\link{workplan}()}
#' @examples
#' # Create example workflow plan data frames for make()
#' workplan(small = simulate(5), large = simulate(50))
#' workplan(list = c(x = "1 + 1", y = "sqrt(x)"))
#' workplan(data = readRDS("my_data.rds"))
#' workplan(my_file.rds = saveRDS(1+1, "my_file.rds"), file_targets = TRUE,
#'   strings_in_dots = "literals")
workplan <- function(
  ...,
  list = character(0),
  file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")
){
  strings_in_dots <- match.arg(strings_in_dots)
  dots <- match.call(expand.dots = FALSE)$...
  commands_dots <- lapply(dots, wide_deparse)
  names(commands_dots) <- names(dots)
  commands <- c(commands_dots, list)
  targets <- names(commands)
  commands <- as.character(commands)
  if (!length(commands)){
    return(
      data.frame(
        target = character(0),
        command = character(0)
      )
    )
  }
  plan <- data.frame(
    target = targets,
    command = commands,
    stringsAsFactors = FALSE
  )
  from_dots <- plan$target %in% names(commands_dots)
  if (file_targets){
    plan$target <- drake::drake_quotes(plan$target, single = TRUE)
  }
  if (strings_in_dots == "filenames"){
    plan$command[from_dots] <- gsub("\"", "'", plan$command[from_dots])
  }
  sanitize_plan(plan)
}

#' @title Function \code{as_drake_filename}
#' @description Converts an ordinary character string
#' into a filename understandable by drake. In other words,
#' \code{as_drake_filename(x)} just wraps single quotes around \code{x}.
#' @export
#' @return A single-quoted character string: i.e., a filename
#' understandable by drake.
#' @param x character string to be turned into a filename
#' understandable by drake (i.e., a string with literal
#' single quotes on both ends).
#' @examples
#' # Wraps the string in single quotes.
#' as_drake_filename("my_file.rds") # "'my_file.rds'"
as_drake_filename <- function(x){
  drake::drake_quotes(x, single = TRUE)
}

wide_deparse <- function(x){
  paste(deparse(x), collapse = "")
}

workplan_override <- function(target, field, config){
  in_plan <- config$plan[[field]]
  if (is.null(in_plan)){
    return(config[[field]])
  } else {
    return(in_plan[config$plan$target == target])
  }
}
