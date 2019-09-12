#' @title Declare existing scripts to be run
#' \lifecycle{experimental}
#' @description `code_to_function()` parses individual \*.R/\*.RMD files
#'   to be added into the drake workflow
#'
#' @export
#' @inheritSection drake_plan Keywords
#' @seealso [file_in()], [file_out()], [knitr_in()], [ignore()], [no_deps()]
#' @return An expression to be input into the drake plan
#' @param path Character vector, path to script.
#' @param ... unevaluated character values detailing the prior dependencies the script has.
#' @export
#' @examples
#' \dontrun{
#' isolate_example("contain side effects", {
#' # The `code_to_function()` function creates an expression that makes it available for drake to
#' # process as part of the workflow,
#' # The main purpose is to allow pre-exising workflows to
#' # incorporate drake into the workflow seamlessly for cases where re-factoring is unfeasible
#' #
#'
#' script1 <- tempfile()
#' script2 <- tempfile()
#' script3 <- tempfile()
#' script4 <- tempfile()
#' writeLines(c("data <- mtcars", "munge(data)"), script1)
#' writeLines("analyze(munged)", script2)
#' writeLines("summarize_results(analysis)", script3)
#' writeLines("plot_results(analysis)", script4)
#'
#'
#' do_munge<-code_to_function(script1)
#' do_analysis<-code_to_function(script2)
#' do_summarize<-code_to_function(script3)
#' do_viz<-code_to_function(script4)
#'
#' plan <- drake_plan(
#'   munged   = do_munge(),
#'   analysis = do_analysis(munged),
#'   summary  = do_summarize(analysis),
#'   plot     = do_viz(analysis)
#'  )
#'
#' plan
#' # drake knows  "script1" is the first script to be evaluated and ran, because it has no dependencies on other code
#' # and a dependency of `analysis`. See for yourself:
#'
#' make(plan)
#'
#' # See the connections that the sourced scripts create:
#' config <- drake_config(plan)
#' if (requireNamespace("visNetwork", quietly = TRUE)) {
#'   vis_drake_graph(config)
#' }
#' })
#' }

code_to_function <- function(path,...) {
  lines <- readLines(path)
  knitr_pattern <- "^(### chunk number|<<[^>]*>>=|```\\{r.*\\})"
  if (any(grepl(knitr_pattern, lines))) {
    lines <- get_tangled_text(path)
  }
  source_digest<-digest::digest(lines)
  lines <- c(
    "function(...){",
    lines,
    paste0("\"",source_digest,"\""), #ensures next step gets run if the code changes, we can relook at this step
    "}"
  )
  text <- paste(lines, sep = "\n")

  eval(safe_parse(text))

}
