#' @title Turn an R script file or knitr / R Markdown report
#'   into a `drake` workflow plan data frame.
#' @export
#' @seealso [drake_plan()], [make()]
#' @description In your script or `knitr` / R Markdown code chunks,
#'   you can assign expressions to variables,
#'   and `code_to_plan()` will turn them into commands and targets,
#'   respectively.
#' @details This feature is easy to break, so there are some rules
#'   for your code file:
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
#' @param path a file path to an R script or `knitr` report.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' drake_example("code_to_plan") # Download some code files to make plans.
#' # Read the code and Markdown files in the downloaded "code_to_plan"
#' # folder for details.
#' plan <- code_to_plan("code_to_plan/script.R")
#' plan2 <- code_to_plan("code_to_plan/report.Rmd")
#' identical(plan, plan2) # should be TRUE
#' print(plan)
#' make(plan)
#' readd(discrepancy)
#' })
#' }
code_to_plan <- function(path){
  assert_pkg("CodeDepends", install = "BiocManager::install")
  nodes <- CodeDepends::getInputs(CodeDepends::readScript(path))
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
