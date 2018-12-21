#' @title Put quotes around each element of a character vector.
#' @description Quotes are important in drake.
#' In workflow plan data frame commands,
#' single-quoted targets denote physical files,
#' and double-quoted strings are treated as ordinary string literals.
#' @seealso [drake_unquote()], [drake_strings()]
#' @export
#' @return Character vector with quotes around it.
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
#' @return Character vector without leading
#'   or trailing escaped quotes around
#'   the elements.
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
#' @return A character vector.
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

is_quoted <- function(x) {
  x <- substr(x = x, start = 0, stop = 1)
  x == "\"" | x == "'"
}

not_quoted <- function(x) {
  !is_quoted(x)
}

quotes_regex <- "^(?:'(.*)'|\"(.*)\")$"
