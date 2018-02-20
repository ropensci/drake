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
#' use the [file_in()], [knitr_in()], and
#' [file_out()] functions in your commands.
#' the examples in this help file provide some guidance.
#' @export
#' @return A data frame of targets and commands.
#' @param ... A collection of symbols/targets
#'   with commands assigned to them. See the examples for details.
#' @param list A named character vector of commands
#'   with names as targets.
#' @param file_targets deprecated argument. See [file_out()],
#'   [file_in()], and [knitr_in()] for the current way to work
#'   with files.
#'   In the past, this argument was a logical to indicate whether the
#'   target names should be single-quoted to denote files. But the newer
#'   interface is much better.
#' @param strings_in_dots deprecated argument. See [file_out()],
#'   [file_in()], and [knitr_in()] for the current way to work
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
#'   write.csv(mtcars[, c("mpg", "cyl")], file_out("mtcars.csv")),
#'   value = read.csv(file_in("mtcars.csv"))
#' )
#' mtcars_plan
#' make(mtcars_plan) # Makes `mtcars.csv` and then `value`
#' head(readd(value))
#' # You can use knitr inputs too. See the top command below.
#' load_basic_example()
#' head(my_plan)
#' # The `knitr_in("report.Rmd")` tells `drake` to dive into the active
#' # code chunks to find dependencies.
#' # There, `drake` sees that `small`, `large`, and `coef_regression2_small`
#' # are loaded in with calls to `loadd()` and `readd()`.
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
  if (length(file_targets) || identical(strings_in_dots, "filenames")){
    warning(
      "Use the file_in(), file_out(), and knitr_in() functions ",
      "to work with files in your commands. ",
      "See `?drake_plan` for examples. ",
      "The `file_targets` and `strings_in_target` are deprecated. ",
      "Worry about single-quotes no more!"
    )
  }
  if (identical(file_targets, TRUE)){
    plan$target <- drake::drake_quotes(plan$target, single = TRUE)
  }
  # TODO: leave double-quoted strings alone when we're ready to
  # deprecate single-quoting in the file API.
  # Currently, to totally take advantage of the new file API,
  # users need to set strings_in_dots to "filenames" every time.
  if (!length(strings_in_dots) || strings_in_dots == "filenames"){
    plan$command[from_dots] <- gsub("\"", "'", plan$command[from_dots])
  }
  sanitize_plan(plan)
}

complete_target_names <- function(commands_list){
  if (!length(names(commands_list))){
    # Should not actually happen, but it's better to have anyway.
    names(commands_list) <- paste0("drake_target_", seq_along(commands_list)) # nocov # nolint
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
    return(in_plan[[which(config$plan$target == target)]])
  }
}

#' @title Declare the file inputs of a workflow plan command.
#' @description Use this function to help write the commands
#'   in your workflow plan data frame. See the examples
#'   for a full explanation.
#' @export
#' @seealso `file_out` `knitr_in`
#' @return A character vector of declared input file paths.
#' @param ... Character strings. File paths of input files
#'   to a command in your workflow plan data frame.
#' @export
#' @examples
#' \dontrun{
#' test_with_dir("Contain side effects", {
#' # The `file_out()` and `file_in()` functions
#' # just takes in strings and returns them.
#' file_out("summaries.txt")
#' # Their main purpose is to orchestrate your custom files
#' # in your workflow plan data frame.
#' suppressWarnings(
#'   plan <- drake_plan(
#'     write.csv(mtcars, file_out("mtcars.csv")),
#'     contents = read.csv(file_in("mtcars.csv")),
#'     strings_in_dots = "literals" # deprecated but useful: no single quotes needed. # nolint
#'   )
#' )
#' plan
#' # Drake knows "\"mtcars.csv\"" is the first target
#' # and a dependency of `contents`. See for yourself:
#' config <- make(plan)
#' file.exists("mtcars.csv")
#' vis_drake_graph(config)
#' # See also `knitr_in()`. `knitr_in()` is like `file_in()`
#' # except that it analyzes active code chunks in your `knitr`
#' # source file and detects non-file dependencies.
#' # That way, updates to the right dependencies trigger rebuilds
#' # in your report.
#' })
#' }
file_in <- function(...){
  as.character(c(...))
}

#' @title Declare the file outputs of a workflow plan command.
#' @description Use this function to help write the commands
#'   in your workflow plan data frame. You can only specify
#'   one file output per command. See the examples
#'   for a full explanation.
#' @export
#' @seealso file_in knitr_in
#' @return A character string, the file path of the file output.
#' @param path Character string of length 1. File path
#'   of the file output of a command in your
#'   workflow plan data frame.
#' @param ... Do not use. For informative input handling only.
#' @examples
#' \dontrun{
#' test_with_dir("Contain side effects", {
#' # The `file_out()` and `file_in()` functions
#' # just takes in strings and returns them.
#' file_out("summaries.txt")
#' # Their main purpose is to orchestrate your custom files
#' # in your workflow plan data frame.
#' suppressWarnings(
#'   plan <- drake_plan(
#'     write.csv(mtcars, file_out("mtcars.csv")),
#'     contents = read.csv(file_in("mtcars.csv")),
#'     strings_in_dots = "literals" # deprecated but useful: no single quotes needed. # nolint
#'   )
#' )
#' plan
#' # Drake knows "\"mtcars.csv\"" is the first target
#' # and a dependency of `contents`. See for yourself:
#' config <- make(plan)
#' file.exists("mtcars.csv")
#' vis_drake_graph(config)
#' # See also `knitr_in()`. `knitr_in()` is like `file_in()`
#' # except that it analyzes active code chunks in your `knitr`
#' # source file and detects non-file dependencies.
#' # That way, updates to the right dependencies trigger rebuilds
#' # in your report.
#' })
#' }
file_out <- function(path){
  if (length(path) != 1){
    warning(
      "In file_out(), the `path` argument must ",
      "have length 1. Supplied length = ", length(path), ". ",
      "using first file output: ", path[1], ".",
      call. = FALSE
    )
  }
  as.character(path[1])
}

#' @title Declare the `knitr`/`rmarkdown` source files
#'   of a workflow plan command.
#' @description Use this function to help write the commands
#'   in your workflow plan data frame. See the examples
#'   for a full explanation.
#' @export
#' @seealso file_in file_out
#' @return A character vector of declared input file paths.
#' @param ... Character strings. File paths of `knitr`/`rmarkdown`
#'   source files suplied to a command in your workflow plan data frame.
#' @examples
#' \dontrun{
#' test_with_dir("Contain side effects", {
#' # `knitr_in()` is like `file_in()`
#' # except that it analyzes active code chunks in your `knitr`
#' # source file and detects non-file dependencies.
#' # That way, updates to the right dependencies trigger rebuilds
#' # in your report.
#' # The basic example (`drake_example("basic")`)
#' # already has a demonstration
#' load_basic_example()
#' config <- make(my_plan)
#' vis_drake_graph(config)
#' # Now how did drake magically know that
#' # `small`, `large`, and `coef_regression2_small` were
#' # dependencies of the output file `report.md`?
#' # because the command in the workflow plan had
#' # `knitr_in("report.Rmd")` in it, so drake knew
#' # to analyze the active code chunks. There, it spotted
#' # where `small`, `large`, and `coef_regression2_small`
#' # were read from the cache using calls to `loadd()` and `readd()`.
#' })
#' }
knitr_in <- file_in
