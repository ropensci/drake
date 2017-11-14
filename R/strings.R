# All functions in this file are taken from eply:
# https://github.com/wlandau-lilly/eply

#' @title Function \code{drake_quotes}
#' @description Put quotes around each element of a character vector.
#' @seealso \code{\link{drake_unquote}}, \code{\link{drake_strings}}
#' @export
#' @return character vector with quotes around it
#' @param x character vector or object to be coerced to character.
#' @param single Add single quotes if \code{TRUE}
#' and double quotes otherwise.
#' @examples
#' # Single-quote this string.
#' drake_quotes("abcd", single = TRUE) # "'abcd'"
#' # Double-quote this string.
#' drake_quotes("abcd") # "\"abcd\""
drake_quotes <- function(x = NULL, single = FALSE){
  stopifnot(is.logical(single))
  if (single){
    paste0("'", x, "'")
  } else {
    paste0("\"", x, "\"")
  }
}

#' @title Function \code{drake_unquote}
#' @description Remove leading and trailing
#' escaped quotes from character strings.
#' @seealso \code{\link{drake_quotes}}, \code{\link{drake_strings}}
#' @export
#' @return character vector without leading
#' or trailing escaped quotes around
#' the elements
#' @param x character vector
#' @param deep remove all outer quotes if \code{TRUE}
#' and only the outermost set otherwise. Single and double
#' quotes are treated interchangeably, and matching is not checked.
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

#' @title Function \code{drake_strings}
#' @description Turn valid expressions into character strings.
#' @seealso \code{\link{drake_quotes}}, \code{\link{drake_unquote}}
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
