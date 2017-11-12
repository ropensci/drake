#' @title Function knitr_deps
#' @export
#' @seealso \code{\link{knitr_deps}},
#' \code{\link{deps}},
#' \code{\link{make}}, \code{\link{load_basic_example}}
#' @description Find the dependencies of a dynamic report. To
#' enable drake to watch for these dependencies, your workplan
#' plan command to compile this report must make direct use of
#' \code{knitr::knit()}.
#' That is, it must look something like \code{knit('your_report.Rmd')}
#' in your workflow plan data frame.
#' @return A character vector of the names of dependencies.
#' @details Drake looks for dependencies in the document by
#' analyzing evaluated code chunks for other targets/imports
#' mentioned in \code{\link{loadd}()} and \code{\link{readd}()}.
#' @param target file path to the file or name of the file target,
#' source text of the document.
#' @examples
#' \dontrun{
#' load_basic_example() # Load the canonical example of drake.
#' knitr_deps("'report.Rmd'") # Files must be single-quoted.
#' # Find the dependencies of the compiled output target, 'report.md'.
#' knitr_deps("report.Rmd")
#' make(my_plan) # Run the project.
#' knitr_deps("'report.md'") # Work on the Rmd source, not the output.
#' }
knitr_deps <- function(target){
  if (!length(target)){
    return(character(0))
  }
  file <- unquote(target)
  if (!file.exists(file)){
    warning(
      "dynamic report '", file,
      "' does not exist and cannot be inspected for dependencies.",
      call. = FALSE
    )
    return(character(0))
  }
  fragments <- get_tangled_frags(file)
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
    if (is_function_call(expr, package = "knitr", what = "knit")){
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
    input_index <- min(which(!nchar(names(args))))
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
  package = c("drake", "knitr"),
  what = c("knit", "loadd", "readd")
){
  package <- match.arg(package)
  what <- match.arg(what)
  eply::unquote(deparse(expr[[1]])) %in%
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
