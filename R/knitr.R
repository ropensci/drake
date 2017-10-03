#' @title Function dknit
#' @export
#' @seealso \code{\link{deps_in_document}},
#' \code{\link{deps}}, \code{\link{deps_in_document}},
#' \code{\link{make}}, \code{\link{load_basic_example}}
#' @description In a drake workflow, use the command
#' \code{dknit("'your_document.Rmd'")} to knit the file
#' \code{your_document.Rmd} while automatically accounting for the
#' dependencies in \code{your_document.Rmd}. In the document,
#' for any targerts/imports specifically invoked in calls to
#' \code{\link{readd}()} and \code{\link{loadd}()} inside
#' evaluated code chunks, drake will watch these items for changes.
#' When any one of them changes, \code{\link{make}()} will
#' recompile your document.
#' @param input charadcter scalar, name of an imported dynamic document
#' source file. The \code{input} is passed as the \code{input}
#' argument to \code{knitr::knit}, but it must be
#' enclosed in single quotes just like any other file target/import
#' in drake. The file must also already exist beforehand, so it
#' must be an imported file, not a built target.
#' @param ... other arguments to \code{knitr::knit}.
#' @examples
#' \dontrun{
#' load_basic_example
#' print(my_plan) # Notice: dknit('report.Rmd') makes 'report.md'.
#' plot_graph(my_plan) # 'report.md' depends on mentions in 'report.Rmd'.
#' make(my_plan) # First build.
#' make(my_plan) # Everything up to date.
#' reg2 <- function(d){
#'   d$x3 <- d$x ^ 3
#'   lm(y ~ x3, data = d)
#' }
#' make(my_plan) # 'report.md' should rebuild.
#' }
dknit <- function(input, ...){
  if (!is_file(input)){
    stop(
      "In dknit(), the name of the file must be enclosed in single quotes ",
      "just like file targets/imports in the workflow plan data frame."
    )
  }
  knitr::knit(input = eply::unquote(input), ...)
}

find_dknit_doc <- function(expr, result = character(0)){
  if (is.character(expr)){
    expr <- parse(text = expr)
  }
  if (is.function(expr)){
    result <- find_dknit_doc(body(expr), result = result)
  } else if (is.call(expr) & length(expr) > 1){
    if (is_drake_function_call(expr, what = "dknit")){
      result <- doc_of_function_call(expr)
    } else {
      result <- lapply(as.list(expr), find_dknit_doc,
        result = result) %>%
        clean_dependency_list
    }
  } else if (is.recursive(expr)){
    result <- lapply(as.list(expr), find_dknit_doc,
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

#' @title Function deps_in_document
#' @export
#' @seealso \code{\link{deps_in_document}},
#' \code{\link{deps}}, \code{\link{deps_in_document}},
#' \code{\link{make}}, \code{\link{load_basic_example}}
#' @description Find the dependencies of a dynamic report. To
#' enable drake to watch for these dependencies, your command
#' to compile this report must be use \code{\link{dknit}}:
#' that is, it must look something like \code{dknit('your_report.Rmd')}
#' in your workflow plan data frame.
#' @details Drake looks for dependencies in the document by
#' analyzing evaluated code chunks for other targets/imports
#' mentioned in \code{\link{loadd}()} and \code{\link{readd}()}.
#' @param target file path to the file or name of the file target,
#' source text of the document.
#' @examples
#' \dontrun{
#' load_basic_example()
#' deps_in_document("'report.Rmd'")
#' deps_in_document("report.md")
#' }
deps_in_document <- function(target){
  file <- unquote(target)
  fragments <- get_tangled_frags(file)
  sort(find_knitr_targets(fragments))
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
  if (!is_drake_function_call(expr, what = "loadd")){
    return()
  }
  args <- as.list(expr)[-1]
  targets <- unnamed_in_list(args)
  list <- get_specific_arg(args = args, name = "list")
  c(targets, list)
}

analyze_readd <- function(expr){
  if (!is_drake_function_call(expr, what = "readd")){
    return()
  }
  args <- as.list(expr)[-1]
  targets <- unnamed_in_list(args)
  target <- get_specific_arg(args = args, name = "target")
  c(targets, target)
}

is_drake_function_call <- function(expr,
  what = c("dknit", "loadd", "readd")
){
  what <- match.arg(what)
  eply::unquote(deparse(expr[[1]])) %in%
    paste0(c("", "drake::", "drake:::"), what)
}

found_loadd_readd <- function(x){
  grepl("readd|loadd", wide_deparse(x))
}

get_specific_arg <- function(args, name){
  tryCatch(
    eval(args[[name]]),
    error = function(e){
      character(0)
    }
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
