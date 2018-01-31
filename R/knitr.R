#' @title Find the drake dependencies of a dynamic knitr report target.
#' @export
#' @seealso [deps()],
#' [make()], [load_basic_example()]
#' @description To enable drake to watch for the dependencies
#' of a knitr report, the command in your workflow plan data frame
#' must call [knitr::knit()] directly.
#' In other words,
#' the command must look something like
#' `knit('your_report.Rmd')` or
#' `knit('your_report.Rmd', quiet = TRUE)`.
#' @return A character vector of the names of dependencies.
#' @details Drake looks for dependencies in the document by
#' analyzing evaluated code chunks for other targets/imports
#' mentioned in [loadd()] and [readd()].
#' @param target file path to the file or name of the file target,
#' source text of the document.
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
        "'. Drake dependencies could not be extracted from code chunks."
      )
      character(0)
    }
  )
  sort(find_knitr_targets(fragments))
}

find_knitr_doc <- function(expr, result = character(0)){
  if (!length(expr)){
    return(result)
  }
  if (is.character(expr)) {
    if (is_parsable(expr)) {
      expr <- parse(text = expr)
    } else {
      return(result)
    }
  }
  if (is.function(expr)){
    result <- find_knitr_doc(body(expr), result = result)
  } else if (is.call(expr) & length(expr) > 1){
    does_knitting <-
      is_function_call(expr, package = "knitr", what = "knit") ||
      is_function_call(expr, package = "rmarkdown", what = "render")
    if (does_knitting){
      result <- doc_of_function_call(expr)
    } else {
      result <- lapply(as.list(expr), find_knitr_doc,
        result = result) %>%
        clean_dependency_list
    }
  } else if (is.recursive(expr)){
    result <- lapply(as.list(expr), find_knitr_doc,
      result = result) %>%
      clean_dependency_list
  }
  result
}

doc_of_function_call <- function(expr){
  args <- as.list(expr)[-1]
  if (!length(args)){
    return(character(0))
  }
  if (is.null(names(args))){
    names(args) <- rep("", length(args))
  }
  if (!is.null(args$input)){
    as.character(args$input)
  } else {
    unnamed <- which(!nchar(names(args)))
    if (!length(unnamed)){
      return(character(0))
    }
    input_index <- min(unnamed)
    as.character(args[[input_index]])
  }
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

find_knitr_targets <- function(expr, targets = character(0)){
  if (is.function(expr)){
    targets <- find_knitr_targets(body(expr), targets = targets)
  } else if (is.call(expr) & length(expr) > 1){
    targets <- c(targets, analyze_loadd(expr), analyze_readd(expr))
    targets <- as.list(expr) %>%
      Filter(f = found_loadd_readd) %>%
      lapply_find_knitr_targets(targets = targets)
  } else if (is.recursive(expr)){
    targets <- lapply_find_knitr_targets(
      list = as.list(expr), targets = targets)
  }
  targets
}

analyze_loadd <- function(expr){
  if (!is_function_call(expr, package = "drake", what = "loadd")){
    return()
  }
  args <- as.list(expr)[-1]
  targets <- unnamed_in_list(args)
  list <- get_specific_arg(args = args, name = "list")
  c(targets, list)
}

analyze_readd <- function(expr){
  if (!is_function_call(expr, package = "drake", what = "readd")){
    return()
  }
  args <- as.list(expr)[-1]
  targets <- unnamed_in_list(args)
  target <- get_specific_arg(args = args, name = "target")
  c(targets, target)
}

is_function_call <- function(
  expr,
  package = c("drake", "knitr", "rmarkdown"),
  what = c("loadd", "readd", "knit", "render")
){
  package <- match.arg(package)
  what <- match.arg(what)
  drake::drake_unquote(deparse(expr[[1]])) %in%
    paste0(c("", paste0(package, c("::", ":::"))), what)
}

found_loadd_readd <- function(x){
  grepl("readd|loadd", wide_deparse(x))
}

get_specific_arg <- function(args, name){
  tryCatch(
    eval(args[[name]]),
    error = error_character0
  )
}

lapply_find_knitr_targets <- function(list, targets){
  v <- lapply(list, find_knitr_targets, targets = targets)
  targets <- unique(c(targets, unlist(v)))
}

unnamed_in_list <- function(x){
  if (!length(names(x))){
    as.character(x)
  } else {
    as.character(x[!nchar(names(x))])
  }
}
