safe_grepl <- function(pattern, x){
  tryCatch(grepl(pattern, x), error = error_false)
}

is_file <- function(x){
  (safe_grepl("^'", x) & safe_grepl("'$", x)) |
    (safe_grepl("^\"", x) & safe_grepl("\"$", x))
}

standardize_filename <- function(text){
  text[is_file(text)] <-  gsub("^'|'$", "\"", text[is_file(text)])
  text
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

merge_lists <- function(x, y){
  names <- base::union(names(x), names(y))
  lapply(
    X = names,
    function(name){
      c(x[[name]], y[[name]])
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
    unique() %>%
    sort()
}
