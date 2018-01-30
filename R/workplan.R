#' @title Create a workflow plan data frame
#' for the \code{plan} argument of \code{\link{make}}.
#' @description Turns a named collection of target/command pairs into
#' a workflow plan data frame for \code{\link{make}()} and
#' \code{\link{check}()}. You can give the commands
#' as named expressions, or you can use the \code{list}
#' argument to supply them as character strings.
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
#' @param ... A collection of symbols/targets
#' with commands assigned to them. See the examples for details.
#' @param list A named list of targets, where the values
#' are commands.
#' @param file_targets logical, whether the targets should be
#' (single-quoted) external file targets.
#' @param strings_in_dots Character scalar,
#' how to treat quoted character strings in the commands
#' specified through \code{...}.
#' Set to \code{"filenames"} to treat all these strings as
#' external file targets/imports (single-quoted),
#' or to \code{"literals"} to treat them all as literal
#' strings (double-quoted).
#' Unfortunately, because of how R deparses code,
#' you cannot simply leave literal quotes alone in the
#' \code{...} argument. R will either convert all these quotes
#' to single quotes or double quotes. Literal quotes in the
#' \code{list} argument are left alone.
#' @examples
#' # Create example workflow plan data frames for make()
#' drake_plan(small = simulate(5), large = simulate(50))
#' # Commands can be multi-line code chunks.
#' small_plan <- drake_plan(
#'   small_target = {
#'     local_object <- 1 + 1
#'     2 + sqrt(local_object)
#'   }
#' )
#' small_plan
#' cache <- storr::storr_environment() # Avoid writing files in examples.
#' make(small_plan, cache = cache) # Most users don't need the cache argument.
#' cached(cache = cache)
#' readd(small_target, cache = cache)
#' # local_object only applies to the code chunk.
#' ls() # your environment is protected (local_object not found)
#' rm(small_target)
#' # For tighter control over commands, use the `list` argument.
#' # Single quotes are file names, double quotes are oridinary strings.
#' # To actually run the little example workflow below,
#' # you need a file called `my_file.xlsx``
#' # with an Excel sheet called `second_sheet`.
#' # You also need to install the xlsx package.
#' drake_plan(
#'   list = c(
#'     dataset = "readxl::read_excel(path = 'my_file.xlsx', sheet = \"second_sheet\")" # nolint
#'   )
#' )
#' # Output targets can also be files,
#' # but the target names must have single quotes around them.
#' mtcars_plan <- drake_plan(
#'   output_file.csv = write.csv(mtcars, "output_file.csv"),
#'   file_targets = TRUE,
#'   strings_in_dots = "literals"
#' )
#' mtcars_plan
#' # make(mtcars_plan) # Would write output_file.csv. # nolint
#' # In the free-form `...` argument
#' # drake_plan() uses tidy evaluation to figure out your commands.
#' # For example, it respects the quasiquotation operator `!!`
#' # when it figures out what your code should be.
#' # Suppress this with the `list` argument.
#' my_variable <- 5
#' drake_plan(
#'   a = !!my_variable,
#'   b = !!my_variable + 1,
#'   list = c(d = "!!my_variable")
#' )
#' # For instances of !! that remain unevaluated in the workflow plan,
#' # make() will run these commands in tidy fashion,
#' # evaluating the !! operator using the environment you provided.
drake_plan <- function(
  ...,
  list = character(0),
  file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")
){
  strings_in_dots <- match.arg(strings_in_dots)
  dots <- rlang::exprs(...) # Enables quasiquotation via rlang.
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

drake_plan_override <- function(target, field, config){
  in_plan <- config$plan[[field]]
  if (is.null(in_plan)){
    return(config[[field]])
  } else {
    return(in_plan[config$plan$target == target])
  }
}
