drake_defunct <- function(...) {
  .Defunct(
    new = "",
    package = "drake",
    msg = paste0("function ", match.call()[[1]], "() in drake is defunct.")
  )
}

# nocov start

#' @title from_plan \lifecycle{defunct}
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

#' @title analyses \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
analyses <- drake_defunct

#' @title as_drake_filename \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
as_drake_filename <- drake_defunct

#' @title as_file \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
as_file <- drake_defunct

#' @title backend \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
backend <- drake_defunct

#' @title build_graph \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
build_graph <- drake_defunct

#' @title check \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
check <- drake_defunct

#' @title config \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
config <- drake_defunct

#' @title dataframes_graph \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
dataframes_graph <- drake_defunct

#' @title default_system2_args \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
default_system2_args <- drake_defunct

#' @title deprecate_wildcard \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
deprecate_wildcard <- drake_defunct

#' @title deps \lifecycle{defunct}
#' @description 2019-05-16
#' @export
#' @keywords internal
#' @param ... Arguments
deps <- drake_defunct

#' @title doc_of_function_call \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
doc_of_function_call <- drake_defunct

#' @title evaluate \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
evaluate <- drake_defunct

#' @title example_drake \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
example_drake <- drake_defunct

#' @title examples_drake \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
examples_drake <- drake_defunct

#' @title expand \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
expand <- drake_defunct

#' @title gather \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
gather <- drake_defunct

#' @title find_knitr_doc \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
find_knitr_doc <- drake_defunct

#' @title is_function_call \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
is_function_call <- drake_defunct

#' @title load_basic_example \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
load_basic_example <- drake_defunct

#' @title max_useful_jobs \lifecycle{defunct}
#' @description 2019-05-16
#' @export
#' @keywords internal
#' @param ... Arguments
max_useful_jobs <- drake_defunct

#' @title migrate_drake_project \lifecycle{defunct}
#' @description 2019-05-16
#' @export
#' @keywords internal
#' @param ... Arguments
migrate_drake_project <- drake_defunct

#' @title parallel_stages \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
parallel_stages <- drake_defunct

#' @title plan \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
plan <- drake_defunct

#' @title plan_drake \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
plan_drake <- drake_defunct

#' @title plot_graph \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
plot_graph <- drake_defunct

#' @title rate_limiting_times \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
rate_limiting_times <- drake_defunct

#' @title read_config \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
read_config <- drake_defunct

#' @title read_graph \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
read_graph <- drake_defunct

#' @title read_drake_meta \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
read_drake_meta <- drake_defunct

#' @title read_plan \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
read_plan <- drake_defunct

#' @title render_graph \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
render_graph <- drake_defunct

#' @title session \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
session <- drake_defunct

#' @title summaries \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
summaries <- drake_defunct

#' @title workflow \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
workflow <- drake_defunct

#' @title workplan \lifecycle{defunct}
#' @description 2019-02-15
#' @export
#' @keywords internal
#' @param ... Arguments
workplan <- drake_defunct

# nocov end
