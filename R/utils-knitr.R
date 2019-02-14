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
#' test_with_dir("Quarantine side effects.", {
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

safe_get_tangled_frags <- function(file) {
  if (!length(file)) {
    return(character(0))
  }
  if (!file.exists(file)) {
    warning(
      "knitr/rmarkdown report '", file,
      "' does not exist and cannot be inspected for dependencies.",
      call. = FALSE
    )
    return(character(0))
  }
  fragments <- tryCatch({
    get_tangled_frags(file)
  },
  error = function(e) {
    warning(
      "Could not parse file '", file,
      "'. drake dependencies could not be extracted from code chunks: ",
      conditionMessage(e)
    )
    character(0)
  })
}

# From https://github.com/duncantl/CodeDepends/blob/master/R/sweave.R#L15
get_tangled_frags <- function(doc) {
  assert_pkg("knitr")
  id <- make.names(tempfile())
  con <- textConnection(id, "w", local = TRUE)
  on.exit(close(con))
  with_options(
    new = list(knitr.purl.inline = TRUE),
    code = knitr::knit(doc, output = con, tangle = TRUE, quiet = TRUE)
  )
  code <- textConnectionValue(con)
  parse(text = code)
}
