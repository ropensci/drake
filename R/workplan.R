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
#' @return data frame of targets and command
#' @param ... same as for \code{drake::\link{workplan}()}
#' @param list same as for \code{drake::\link{workplan}()}
#' @param file_targets same as for \code{drake::\link{workplan}()}
#' @param strings_in_dots same as for \code{drake::\link{workplan}()}
#' @examples
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
    plan$target <- eply::quotes(plan$target, single = T)
  }
  if (strings_in_dots == "filenames"){
    plan$command[from_dots] <- gsub("\"", "'", plan$command[from_dots])
  }
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
#' @examples
#' as_file("my_file.rds")
as_file <- function(x){
  eply::quotes(x, single = TRUE)
}

wide_deparse <- function(x){
  paste(deparse(x), collapse = "")
}

sanitize_plan <- function(plan){
  for (field in c("code", "command", "output", "target")){
    if (!is.null(plan[[field]])){
      plan[[field]] <- str_trim(plan[[field]], side = "both")
    }
  }
  as.data.frame(plan, stringsAsFactors = FALSE) %>%
    fix_deprecated_plan_names()
}

sanitize_targets <- function(plan, targets){
  plan <- sanitize_plan(plan)
  targets <- str_trim(targets, side = "both")
  if (!any(targets %in% plan$target)){
    stop("No valid targets requested.", call. = FALSE)
  }
  diffs <- setdiff(targets, plan$target)
  if (length(diffs)){
    warning(
      "Ignoring requested targets not in the workflow plan:\n",
      multiline_message(diffs),
      call. = FALSE
    )
  }
  intersect(targets, plan$target)
}
