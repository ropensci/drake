# From lintr
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) <= 0) {
    y
  } else {
    x
  }
}

assert_pkg <- function(pkg, version = NULL, install = "install.packages") {
  if (!requireNamespace(pkg, quietly = TRUE)){
    stop(
      "package ", pkg, " not installed. ",
      "Please install it with ", install, "(\"", pkg, "\").",
      call. = FALSE
    )
  }
  if (is.null(version)){
    return()
  }
  installed_version <- as.character(utils::packageVersion(pkg))
  is_too_old <- utils::compareVersion(installed_version, version) < 0
  if (is_too_old){
    stop(
      "package ", pkg, " must be version ", version, " or greater. ",
      "Found version ", version, " installed.",
      "Please update it with ", install, "(\"", pkg, "\").",
      call. = FALSE
    )
  }
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

factor_to_character <- function(x){
  if (is.factor(x)){
    x <- as.character(x)
  }
  x
}

file_extn <- function(x){
  splitted <- basename(x) %>%
    strsplit(split = ".", fixed = TRUE) %>%
    unlist() %>%
    rev()
  splitted[1]
}

is_file <- function(x){
  safe_grepl(pattern = quotes_regex, x = x)
}

is_image_filename <- function(x){
  tolower(file_extn(x)) %in% c("jpg", "jpeg", "pdf", "png")
}

is_not_file <- function(x){
  !is_file(x)
}

merge_lists <- function(x, y){
  names <- base::union(names(x), names(y))
  lapply(
    X = names,
    function(name){
      base::union(x[[name]], y[[name]])
    }
  ) %>%
    set_names(nm = names)
}

padded_scale <- function(x){
  r <- range(x)
  pad <- 0.2 * (r[2] - r[1])
  c(r[1] - pad, r[2] + pad)
}

random_tempdir <- function(){
  while (file.exists(dir <- tempfile())){
    Sys.sleep(1e-6) # nocov
  }
  dir.create(dir)
  dir
}

rehash_file_size_cutoff <- 1e5

safe_grepl <- function(pattern, x, ...){
  tryCatch(grepl(pattern, x, ...), error = error_false)
}

safe_is_na <- function(x){
  tryCatch(is.na(x), error = error_false, warning = error_false)
}

select_nonempty <- function(x){
  index <- vapply(
    X = x,
    FUN = function(y){
      length(y) > 0
    },
    FUN.VALUE = logical(1)
  )
  x[index]
}

select_valid <- function(x){
  index <- vapply(
    X = x,
    FUN = function(y){
      length(y) > 0 && !is.na(y)
    },
    FUN.VALUE = logical(1)
  )
  x[index]
}

set_names <- function(x, nm){
  names(x) <- nm
  x
}

standardize_filename <- function(text){
  text[is_file(text)] <-  gsub("^'|'$", "\"", text[is_file(text)])
  text
}
