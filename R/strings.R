# All functions in this file are taken from eply:
# https://github.com/ropensci/eply

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
drake_quotes <- function(x = NULL, single = FALSE){
  stopifnot(is.logical(single))
  if (!length(x)){
    return(character(0))
  }
  if (single){
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
#' @param deep remove all outer quotes if `TRUE`
#'   and only the outermost set otherwise. Single and double
#'   quotes are treated interchangeably, and matching is not checked.
#' @examples
#' x <- "'abcd'"
#' # Remove the literal quotes around x.
#' drake_unquote(x) # "abcd"
drake_unquote <- function(x = NULL, deep = FALSE){
  if (deep){
    gsub("^[\"']*|[\"']*$", "", x)
  } else {
    gsub("^[\"']|[\"']$", "", x)
  }
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
drake_strings <- function(...){
  args <- structure(as.list(match.call()[-1]), class = "uneval")
  keys <- names(args)
  out <- as.character(args)
  names(out) <- keys
  out
}

#' @title Converts an ordinary character string
#'   into a filename understandable by drake.
#' @description This function simply wraps single quotes around `x`.
#' Quotes are important in drake.
#' In workflow plan data frame commands,
#' single-quoted targets denote physical files,
#' and double-quoted strings are treated as ordinary string literals.
#' @export
#' @return A single-quoted character string: i.e., a filename
#'   understandable by drake.
#' @param x character string to be turned into a filename
#'   understandable by drake (i.e., a string with literal
#'   single quotes on both ends).
#' @examples
#'   # Wraps the string in single quotes.
#'   as_drake_filename("my_file.rds") # "'my_file.rds'"
as_drake_filename <- function(x){
  drake::drake_quotes(x, single = FALSE)
}

wide_deparse <- function(x){
  paste(deparse(x), collapse = "\n")
}
