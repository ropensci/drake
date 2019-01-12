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
  if (is.character(x)) {
    x <- parse(text = x, keep.source = FALSE)
  }
  if (is.expression(x) && length(x) == 1L) {
    x <- x[[1]]
  }
  x <- ignore_ignore(x)
  for (attribute in c("srcref", "srcfile", "wholeSrcref")) {
    attr(x = x, which = attribute) <- NULL
  }
  wide_deparse(x)
}

ignore_ignore <- function(x) {
  if (is.function(x) && !is.primitive(x) && !is.null(body(x))) {
    body(x) <- ignore_ignore(body(x))
  } else if (is_callish(x)) {
    if (wide_deparse(x[[1]]) %in% ignored_fns) {
      x <- quote(ignore())
    } else {
      x[] <- lapply(as.list(x), ignore_ignore)
    }
  }
  x
}
