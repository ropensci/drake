safe_grepl <- function(pattern, x){
  tryCatch(grepl(pattern, x), error = error_false)
}

is_file <- function(x){
  safe_grepl("^'", x) & safe_grepl("'$", x)
}

is_existing_file <- function(x){
  is_file(x) & file.exists(drake_unquote(x, deep = TRUE))
}

is_not_file <- function(x){
  !is_file(x)
}

braces <- function(x) {
  paste("{\n", x, "\n}")
}

# From lintr
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) <= 0) {
    y
  } else {
    x
  }
}

drake_select <- function(
  cache, ..., namespaces = cache$default_namespace, list = character(0)
){
  tidyselect::vars_select(
    .vars = list_multiple_namespaces(cache = cache, namespaces = namespaces),
    ...,
    .strict = FALSE
  ) %>%
    unname %>%
    c(list)
}
