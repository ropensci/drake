extract_filenames <- function(command) {
  if (!safe_grepl("'", command, fixed = TRUE)) {
    return(character(0))
  }
  splits <- paste(" ", command, " ")
  splits <- strsplit(splits, split = "'")
  splits <- unlist(splits)
  splits[seq(from = 2, to = length(splits), by = 2)]
}

# Get the command ready for tidy eval prep
# and then pure eval (no side effects).
preprocess_command <- function(command, config) {
  if (is.character(command)){
    command <- parse(text = command)
  }
  command <- as.call(c(quote(`{`), command))
  command <- as.call(c(quote(local), command))
  as.call(c(quote(rlang::expr), command))
}

standardize_command <- function(x) {
  x <- ignore_ignore(x)
  x <- language_to_text(x)
  standardize_code(x)
}

ignore_ignore <- function(expr) {
  if (is.character(expr)) {
    expr <- parse(text = expr)
  }
  recurse_ignore(expr)
}

recurse_ignore <- function(x) {
  if (is.function(x) && !is.primitive(x) && !is.null(body(x))) {
    body(x) <- recurse_ignore(body(x))
  } else if (is_callish(x)) {
    if (wide_deparse(x[[1]]) %in% ignored_fns) {
      x <- quote(ignore())
    } else {
      x[] <- lapply(as.list(x), recurse_ignore)
    }
  }
  x
}

# We won't need this function after #563.
language_to_text <- function(x) {
  if (length(x) < 1) {
    return(character(0)) # nocov
  }
  if (is.expression(x) || is.language(x)) {
    for (attribute in c("srcref", "srcfile", "wholeSrcref")) {
      attr(x = x, which = attribute) <- NULL
    }
    x <- wide_deparse(x)
  }
  x
}

standardize_code <- function(x){
  if (!length(x)){
    return(NA_character_)
  }
  if (is.character(x)) {
    x <- parse(text = x, keep.source = FALSE)[[1]]
  }
  deparse(x)
}
