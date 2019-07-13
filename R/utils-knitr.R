#' @title Find the drake dependencies of a dynamic knitr report target.
#' @export
#' @seealso [deps_code()], [deps_target()]
#' @description Dependencies in `knitr` reports are marked
#'   by [loadd()] and [readd()] in active code chunks.
#' @return A data frame of dependencies.
#' @param path Encoded file path to the `knitr`/R Markdown document.
#'   Wrap paths in [file_store()] to encode.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' deps_knitr("report.Rmd")
#' })
#' }
deps_knitr <- function(path) {
  display_deps_list(decode_deps_list(get_deps_knitr(path)))
}

get_deps_knitr <- function(target) {
  if (!length(target)) {
    return(list())
  }
  out <- new_code_analysis_results()
  if (is_encoded_path(target)) {
    target <- decode_path(target)
  }
  analyze_knitr_file(target, out)
  list_code_analysis_results(out)
}
