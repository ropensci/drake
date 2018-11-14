#' @title Put quotes around each element of a character vector.
#' @description Quotes are important in drake.
#' In workflow plan data frame commands,
#' single-quoted targets denote physical files,
#' and double-quoted strings are treated as ordinary string literals.
#' @seealso [drake_unquote()], [drake_strings()]
#' @export
#' @return character vector with quotes around it
#' @param x character vector or object to be coerced to character.
#' @param single Add single quotes if `TRUE`
#'   and double quotes otherwise.
#' @examples
#' # Single-quote this string.
#' drake_quotes("abcd", single = TRUE) # "'abcd'"
#' # Double-quote this string.
#' drake_quotes("abcd") # "\"abcd\""
drake_quotes <- function(x = NULL, single = FALSE) {
  stopifnot(is.logical(single))
  if (!length(x)) {
    return(character(0))
  }
  if (single) {
    paste0("'", x, "'")
  } else {
    paste0("\"", x, "\"")
  }
}

#' @title Remove leading and trailing
#'   escaped quotes from character strings.
#' @description Quotes are important in drake.
#' In workflow plan data frame commands,
#' single-quoted targets denote physical files,
#' and double-quoted strings are treated as ordinary string literals.
#' @seealso [drake_quotes()], [drake_strings()]
#' @export
#' @return character vector without leading
#'   or trailing escaped quotes around
#'   the elements
#' @param x character vector
#' @param deep deprecated logical.
#' @examples
#' x <- "'abcd'"
#' # Remove the literal quotes around x.
#' drake_unquote(x) # "abcd"
drake_unquote <- function(x = NULL, deep = FALSE) {
  if (deep) {
    warning(
      "The `deep` argument to `drake_unquote()` is deprecated",
      call. = FALSE
    )
  }
  gsub(pattern = quotes_regex, replacement = "\\1\\2", x = x)
}

#' @title Turn valid expressions into character strings.
#' @description This function may be useful for
#'   constructing workflow plan data frames.
#' @seealso [drake_quotes()], [drake_unquote()]
#' @export
#' @return a character vector
#' @param ... unquoted symbols to turn into character strings.
#' @examples
#' # Turn symbols into strings.
#' drake_strings(a, b, c, d) # [1] "a" "b" "c" "d"
drake_strings <- function(...) {
  args <- structure(as.list(match.call()[-1]), class = "uneval")
  keys <- names(args)
  out <- as.character(args)
  names(out) <- keys
  out
}

#' @title Tell `drake` that you want information
#'   on a *file* (target or import), not an ordinary object.
#' @description This function simply wraps literal double quotes around
#'   the argument `x` so `drake` knows it is the name of a file.
#'   Use when you are calling functions like `deps_code()`: for example,
#'   `deps_code(file_store("report.md"))`. See the examples for details.
#'   Internally, `drake` wraps the names of file targets/imports
#'   inside literal double quotes to avoid confusion between
#'   files and generic R objects.
#' @export
#' @return A single-quoted character string: i.e., a filename
#'   understandable by drake.
#' @param x character string to be turned into a filename
#'   understandable by drake (i.e., a string with literal
#'   single quotes on both ends).
#' @examples
#'   # Wraps the string in single quotes.
#'   file_store("my_file.rds") # "'my_file.rds'"
#'   \dontrun{
#'   test_with_dir("contain side effects", {
#'   load_mtcars_example() # Get the code with drake_example("mtcars").
#'   make(my_plan) # Run the workflow to build the targets
#'   list.files() # Should include input "report.Rmd" and output "report.md".
#'   head(readd(small)) # You can use symbols for ordinary objects.
#'   # But if you want to read cached info on files, use `file_store()`.
#'   readd(file_store("report.md"), character_only = TRUE) # File fingerprint.
#'   deps_code(file_store("report.Rmd"))
#'   config <- drake_config(my_plan)
#'   dependency_profile(
#'     file_store("report.Rmd"),
#'     config = config,
#'     character_only = TRUE
#'   )
#'   loadd(list = file_store("report.md"))
#'   get(file_store("report.md"))
#'   })
#'   }
file_store <- function(x) {
  drake::drake_quotes(x, single = FALSE)
}

wide_deparse <- function(x) {
  paste(deparse(x), collapse = "\n")
}

quotes_regex <- "^(?:'(.*)'|\"(.*)\")$"
