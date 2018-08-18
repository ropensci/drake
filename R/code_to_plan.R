#' @title Turn an R script file, knitr / R Markdown report, or quoted R code
#'   into a `drake` workflow plan data frame.
#' @export
#' @seealso [drake_plan()], [make()]
#' @description In your script (or `knitr` / R Markdown code chunks,
#'   or quoted R code), you can assign expressions to variables,
#'   and `code_file_to_plan()` will turn them into commands and targets,
#'   respectively. The `make.R` script demonstrates how this works
#'   for the script `script.R` and the report `report.Rmd`.
#'   The `code_to_plan()` function is similar, but it accepts
#'   quoted code rather than a file.
#' @details This feature is easy to break, so there are some rules:
#'   1. Stick to ssigning a single expression to a single target at a time.
#'     For multi-line commands, please enclose the whole command
#'     in curly braces.
#'     Conversely, compound assignment is not supported
#'     (e.g. `target_1 <- target_2 <- target_3 <- get_data()`).
#'   2. Once you assign an expression to a variable,
#'     do not modify the variable any more.
#'     The target/command binding should be permanent.
#'   3. Keep it simple. Please use the assignment operators rather than 
#'     `assign()` and similar functions.
#' @param code If a character vector, `code` is a file path to an R script
#'   or `knitr` report. Otherwise, `code` should be a language object
#'   or expression, as returned by `quote(x <- 1)`. For more information,
#'   see Hadley Wickham's Advanced R chapter on expressions: 
#'   <http://adv-r.had.co.nz/Expressions.html>.
#' @examples
#' code_to_plan(quote(x <- 1))
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' drake_example("code_to_plan") # Download some code files to make plans.
#' # Read the code and Markdown files in the downloaded "code_to_plan"
#' # folder for details.
#' code_to_plan("code_to_plan/script.R")
#' code_to_plan("code_to_plan/report.Rmd")
#' })
#' }
code_to_plan <- function(code){
  UseMethod("code_to_plan")
}

#' @export
code_to_plan.character <- function(code){
  code_to_plan.default(CodeDepends::readScript(path))
}

#' @export
code_to_plan.default <- function(code){
  assert_pkg("CodeDepends", install = "BiocManager::install")
  nodes <- CodeDepends::getInputs(code)
  if (length(nodes) < 2 || inherits(nodes, "ScriptNodeInfo")){
    nodes <- list(nodes)
  }
  lapply(nodes, node_plan) %>%
    do.call(what = dplyr::bind_rows) %>%
    parse_custom_columns() %>%
    sanitize_plan()
}

node_plan <- function(node){
  code <- node@code
  if (deparse(code[[1]]) %in% c("->", "->>")){
    target <- wide_deparse(code[[3]])
    command <- wide_deparse(code[[2]])
  } else {
    target <- wide_deparse(code[[2]])
    command <- wide_deparse(code[[3]])
  }
  tibble::tibble(
    target = target,
    command = command
  )
}
