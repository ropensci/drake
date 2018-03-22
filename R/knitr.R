#' @title Find the drake dependencies of a dynamic knitr report target.
#' @export
#' @seealso [deps()],
#'   [make()], [load_basic_example()]
#' @description To enable drake to watch for the dependencies
#' of a knitr report, the command in your workflow plan data frame
#' must call [knitr::knit()] directly.
#' In other words,
#' the command must look something like
#' `knit("your_report.Rmd")` or
#' `knit("your_report.Rmd", quiet = TRUE)`.
#' @return A character vector of the names of dependencies.
#' @details Drake looks for dependencies in the document by
#' analyzing evaluated code chunks for other targets/imports
#' mentioned in [loadd()] and [readd()].
#' @param target file path to the file or name of the file target,
#'   source text of the document.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' knitr_deps("'report.Rmd'") # Files must be single-quoted.
#' # Find the dependencies of the compiled output target, 'report.md'.
#' knitr_deps("report.Rmd")
#' make(my_plan) # Run the project.
#' # Work only on the Rmd source, not the output.
#' try(knitr_deps("'report.md'"), silent = FALSE) # error
#' })
#' }
knitr_deps <- function(target){
  if (!length(target)){
    return(character(0))
  }
  knitr_deps_list(target) %>%
    clean_dependency_list
}

knitr_deps_list <- function(target){
  if (!length(target)){
    return(list())
  }
  fragments <- safe_get_tangled_frags(target)
  results <- code_dependencies(fragments)
  select <- c(
    "knitr_in",
    "file_in",
    "file_out",
    "loadd",
    "readd"
  ) %>%
    intersect(y = names(results))
  results[select]
}

safe_get_tangled_frags <- function(target){
  if (!length(target)){
    return(character(0))
  }
  file <- drake_unquote(target)
  if (!file.exists(file)){
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
  error = function(e){
    warning(
      "Could not parse file '", file,
      "'. Drake dependencies could not be extracted from code chunks: ",
      conditionMessage(e)
    )
    character(0)
  })
}

# From https://github.com/duncantl/CodeDepends/blob/master/R/sweave.R#L15
get_tangled_frags <- function(doc, txt = readLines(doc)) {
  in.con <- textConnection(txt)
  out.con <- textConnection("bob", "w", local = TRUE)
  on.exit({
    close(in.con)
    close(out.con)
  })
  knitr::knit(in.con, output = out.con, tangle = TRUE, quiet = TRUE)
  code <- textConnectionValue(out.con)
  parse(text = code)
}
