#' @title Create a workflow plan data frame
#'   for the `plan` argument of [make()].
#' @description Turns a named collection of target/command pairs into
#' a workflow plan data frame for [make()] and
#' [check()]. You can give the commands
#' as named expressions, or you can use the `list`
#' argument to supply them as character strings.
#' @details A workflow plan data frame is a data frame
#' with a `target` column and a `command` column.
#' Targets are the objects and files that drake generates,
#' and commands are the pieces of R code that produce them.
#'
#' To use custom files in your workflow plan,
#' use the [file_input()], [knitr_input()], and
#' [file_output()] functions in your commands.
#' the examples in this help file provide some guidance.
#' @export
#' @return A data frame of targets and commands.
#' @param ... A collection of symbols/targets
#'   with commands assigned to them. See the examples for details.
#' @param list A named character vector of commands
#'   with names as targets.
#' @param file_targets deprecated argument. See [file_output()],
#'   [file_input()], and [knitr_input()] for the new way to work
#'   with files.
#'   In the past, this argument was a logical to indicate whether the
#'   target names should be single-quoted to denote files. But the newer
#'   interface is much better.
#' @param strings_in_dots deprecated argument. See [file_output()],
#'   [file_input()], and [knitr_input()] for the new way to work
#'   with files.
#'   In the past, this argument was a logical to indicate whether the
#'   target names should be single-quoted to denote files. But the newer
#'   interface is much better.
#'
#'   In the past, this argument was a character scalar denoting
#'   how to treat quoted character strings in the commands
#'   specified through `...`.
#'   Set to `"filenames"` to treat all these strings as
#'   external file targets/imports (single-quoted),
#'   or to `"literals"` to treat them all as literal
#'   strings (double-quoted).
#'   Unfortunately, because of how R deparses code,
#'   you cannot simply leave literal quotes alone in the
#'   `...` argument. R will either convert all these quotes
#'   to single quotes or double quotes. Literal quotes in the
#'   `list` argument are left alone.
#' @param tidy_evaluation logical, whether to use tidy evaluation
#'   such as quasiquotation
#'   when evaluating commands passed through the free-form
#'   `...` argument.
#' @examples
#' test_with_dir("Contain side effects", {
#' # Create workflow plan data frames.
#' mtcars_plan <- drake_plan(
#'   write.csv(mtcars[, c("mpg", "cyl")], file_output(mtcars.csv)),
#'   value = read.csv(file_input(mtcars.csv))
#' )
#' mtcars_plan
#' make(mtcars_plan) # Makes `mtcars.csv` and then `value`
#' head(readd(value))
#' # You can use knitr inputs too. See the top command below.
#' load_basic_example()
#' head(my_plan)
#' # The `knitr_input(report.Rmd)` tells `drake` to dive into the active
#' # code chunks to find dependencies. In other words, your
#' # `report.md` file will depend on these targets
#' # loaded into the report itself:
#' deps("report.Rmd")
#' # Are you a fan of tidy evaluation?
#' my_variable <- 1
#' drake_plan(
#'   a = !!my_variable,
#'   b = !!my_variable + 1,
#'   list = c(d = "!!my_variable")
#' )
#' drake_plan(
#'   a = !!my_variable,
#'   b = !!my_variable + 1,
#'   list = c(d = "!!my_variable"),
#'   tidy_evaluation = FALSE
#' )
#' # For instances of !! that remain unevaluated in the workflow plan,
#' # make() will run these commands in tidy fashion,
#' # evaluating the !! operator using the environment you provided.
#' })
drake_plan <- function(
  ...,
  list = character(0),
  file_targets = NULL,
  strings_in_dots = NULL,
  tidy_evaluation = TRUE
){
  if (tidy_evaluation){
    dots <- rlang::exprs(...) # Enables quasiquotation via rlang.
  } else {
    dots <- match.call(expand.dots = FALSE)$...
  }
  commands_dots <- lapply(dots, wide_deparse)
  names(commands_dots) <- names(dots)
  commands <- c(commands_dots, list)
  if (!length(commands)){
    return(
      tibble(
        target = character(0),
        command = character(0)
      )
    )
  }
  commands <- complete_target_names(commands)
  targets <- names(commands)
  commands <- as.character(commands)

  plan <- tibble(
    target = targets,
    command = commands
  )
  from_dots <- plan$target %in% names(commands_dots)
  if (length(file_targets) || length(strings_in_dots)){
    warning(
      "The `file_targets` and `strings_in_target` are deprecated. ",
      "See the help file examples of `drake_plan()` to see the new ",
      "way to handle file inputs/targets. ",
      "Use the file_input(), file_output(), and knitr_input() functions ",
      "in your commands. ",
      "Worry about single-quotes no more!"
    )
  }
  if (length(file_targets) && file_targets){
    plan$target <- drake::drake_quotes(plan$target, single = TRUE)
  }
  if (!length(strings_in_dots) || strings_in_dots == "filenames"){
    plan$command[from_dots] <- gsub("\"", "'", plan$command[from_dots])
  }
  sanitize_plan(plan)
}

complete_target_names <- function(commands_list){
  if (is.null(names(commands_list))){
    names(commands_list) <- paste0("drake_target_", seq_along(commands_list))
  }
  index <- !nchar(names(commands_list))
  names(commands_list)[index] <- paste0("drake_target_", seq_len(sum(index)))
  commands_list
}

drake_plan_override <- function(target, field, config){
  in_plan <- config$plan[[field]]
  if (is.null(in_plan)){
    return(config[[field]])
  } else {
    return(in_plan[config$plan$target == target])
  }
}

file_outputs_to_targets <- function(plan){
  index <- grepl("file_output", plan$command)
  plan$target[index] <- vapply(
    plan$command[index],
    single_file_output,
    character(1)
  )
  plan
}

single_file_output <- function(command){
  file_output <- command_dependencies(command)$file_output
  stopifnot(length(file_output) > 0)
  if (length(file_output) > 1){
    warning(
      "Multiple file outputs found for ", target, ".",
      "Choosing ", file_output[1], " as the target name.",
      call. = FALSE
    )
    file_output[1]
  } else {
    file_output
  }
}
