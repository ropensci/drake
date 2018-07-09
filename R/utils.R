assert_pkgs <- function(pkgs){
  for (pkg in pkgs){
    if (!requireNamespace(pkg, quietly = TRUE)){
      stop(
        "package ", pkg, " not installed. ",
        "Install with install.packages(\"", pkg, "\").",
        call. = FALSE
      )
    }
  }
}

safe_grepl <- function(pattern, x, ...){
  tryCatch(grepl(pattern, x, ...), error = error_false)
}

is_file <- function(x){
  safe_grepl(pattern = quotes_regex, x = x)
}

standardize_filename <- function(text){
  text[is_file(text)] <-  gsub("^'|'$", "\"", text[is_file(text)])
  text
}

is_existing_file <- function(x){
  is_file(x) & file.exists(drake_unquote(x))
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

merge_lists <- function(x, y){
  names <- base::union(names(x), names(y))
  lapply(
    X = names,
    function(name){
      base::union(x[[name]], y[[name]])
    }
  ) %>%
    setNames(nm = names)
}

clean_dependency_list <- function(x){
  if (!length(x)){
    return(character(0))
  }
  x %>%
    unlist() %>%
    unname() %>%
    as.character() %>%
    unique() %>%
    sort()
}

padded_scale <- function(x){
  r <- range(x)
  pad <- 0.2 * (r[2] - r[1])
  c(r[1] - pad, r[2] + pad)
}
