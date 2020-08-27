drake_defunct <- function(...) {
  .Defunct(
    new = "",
    package = "drake",
    msg = paste0(
      "function ",
      safe_deparse(match.call()[[1]]),
      "() in drake is defunct."
    )
  )
}

# nocov start

#' @title from_plan `r lifecycle::badge("defunct")`
#' @description The `from_plan()` function is now defunct
#'   in order to reduce the demands on memory usage.
#' @details 2019-03-28
#' @export
#' @keywords internal
#' @seealso [drake_envir()]
#' @param column Character, name of a column in your `drake` plan.
from_plan <- function(column) {
  .Defunct(
    new = "",
    package = "drake",
    msg = c(
      "Function from_plan() in drake is now defunct ",
      "in order to reduce memory usage."
    )
  )
  NULL
}

#' @title analyses `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
analyses <- drake_defunct

#' @title as_drake_filename `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
as_drake_filename <- drake_defunct

#' @title as_file `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
as_file <- drake_defunct

#' @title backend `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
backend <- drake_defunct

#' @title build_graph `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
build_graph <- drake_defunct

#' @title check `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
check <- drake_defunct

#' @title config `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
config <- drake_defunct

#' @title dataframes_graph `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
dataframes_graph <- drake_defunct

#' @title default_system2_args `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
default_system2_args <- drake_defunct

#' @title deprecate_wildcard `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
deprecate_wildcard <- drake_defunct

#' @title deps `r lifecycle::badge("defunct")`
#' @description 2019-05-16
#' @export
#' @keywords internal
#' @param ... Arguments
deps <- drake_defunct

#' @title doc_of_function_call `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
doc_of_function_call <- drake_defunct

#' @title evaluate `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
evaluate <- drake_defunct

#' @title example_drake `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
example_drake <- drake_defunct

#' @title examples_drake `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
examples_drake <- drake_defunct

#' @title expand `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
expand <- drake_defunct

#' @title gather `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
gather <- drake_defunct

#' @title find_knitr_doc `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
find_knitr_doc <- drake_defunct

#' @title is_function_call `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
is_function_call <- drake_defunct

#' @title load_basic_example `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
load_basic_example <- drake_defunct

#' @title max_useful_jobs `r lifecycle::badge("defunct")`
#' @description 2019-05-16
#' @export
#' @keywords internal
#' @param ... Arguments
max_useful_jobs <- drake_defunct

#' @title migrate_drake_project `r lifecycle::badge("defunct")`
#' @description 2019-05-16
#' @export
#' @keywords internal
#' @param ... Arguments
migrate_drake_project <- drake_defunct

#' @title parallel_stages `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
parallel_stages <- drake_defunct

#' @title plan `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
plan <- drake_defunct

#' @title plan_drake `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
plan_drake <- drake_defunct

#' @title plot_graph `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
plot_graph <- drake_defunct

#' @title rate_limiting_times `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
rate_limiting_times <- drake_defunct

#' @title read_config `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
read_config <- drake_defunct

#' @title read_graph `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
read_graph <- drake_defunct

#' @title read_drake_meta `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
read_drake_meta <- drake_defunct

#' @title read_plan `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
read_plan <- drake_defunct

#' @title render_graph `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
render_graph <- drake_defunct

#' @title session `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
session <- drake_defunct

#' @title summaries `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
summaries <- drake_defunct

#' @title workflow `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
workflow <- drake_defunct

#' @title workplan `r lifecycle::badge("defunct")`
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
workplan <- drake_defunct

# nocov end
