#' @title Find the drake dependencies of a dynamic knitr report target.
#' @export
#' @seealso [deps_code()],
#'   [make()], [load_mtcars_example()]
#' @description To enable drake to watch for the dependencies
#' of a knitr report, the command in your workflow plan data frame
#' must call [knitr::knit()] directly.
#' In other words,
#' the command must look something like
#' `knit("your_report.Rmd")` or
#' `knit("your_report.Rmd", quiet = TRUE)`.
#' @return A character vector of the names of dependencies.
#' @details drake looks for dependencies in the document by
#' analyzing evaluated code chunks for other targets/imports
#' mentioned in [loadd()] and [readd()].
#' @param target file path to the file or name of the file target,
#'   source text of the document.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' knitr_deps(file_store("report.Rmd"))
#' })
#' }
knitr_deps <- function(target) {
  decode_deps_list(get_knitr_deps(target))
}

get_knitr_deps <- function(target) {
  if (!length(target)) {
    return(character(0))
  }
  out <- new_code_analysis_results()
  target <- drake_unquote(target)
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
