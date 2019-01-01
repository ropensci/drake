# Internal encoding API

encode_path <- function(x, config = NULL) {
  if (is.null(config) || is.null(config$ht_encode_path)) {
    reencode_path(x)
  } else {
    ht_memo(ht = config$ht_encode_path, x = x, fun = reencode_path)
  }
}

decode_path <- function(x, config = NULL) {
  if (is.null(config) || is.null(config$ht_decode_path)) {
    redecode_path(x)
  } else {
    ht_memo(ht = config$ht_decode_path, x = x, fun = redecode_path)
  }
}

encode_namespaced <- function(x, config = NULL) {
  if (is.null(config) || is.null(config$ht_encode_namespaced)) {
    reencode_namespaced(x)
  } else {
    ht_memo(
      ht = config$ht_encode_namespaced, x = x, fun = reencode_namespaced
    )
  }
}

decode_namespaced <- function(x, config = NULL) {
  if (is.null(config) || is.null(config$ht_encode_namespaced)) {
    redecode_namespaced(x)
  } else {
    ht_memo(
      ht = config$ht_decode_namespaced, x = x, fun = redecode_namespaced
    )
  }
}

is_encoded_path <- function(x) {
  substr(x = x, start = 1, stop = 2) == "p-"
}

not_encoded_path <- function(x) {
  !is_encoded_path(x)
}

is_encoded_namespaced <- function(x) {
  substr(x = x, start = 1, stop = 2) == "n-"
}

not_encoded_namespaced <- function(x) {
  !is_encoded_namespaced(x)
}

display_keys <- function(x, config = NULL) {
  vapply(
    X = x,
    FUN = display_key,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE,
    config = config
  )
}

decode_deps_list <- function(x) {
  for (field in c("file_in", "file_out", "knitr_in")) {
    if (length(x[[field]])) {
      x[[field]] <- decode_path(x[[field]])
    }
  }
  if (length(x$namespaced)) {
    x$namespaced <- decode_namespaced(x$namespaced)
  }
  x
}

# Do not call the following functions except in the above internal API.

reencode_path <- function(x) {
  y <- base64url::base32_encode(x = x, use.padding = FALSE)
  sprintf("p-%s", y)
}

redecode_path <- function(x) {
  y <- substr(x = x, start = 3, stop = 1e6)
  base64url::base32_decode(x = y, use.padding = FALSE)
}

reencode_namespaced <- function(x) {
  y <- base64url::base32_encode(x, use.padding = FALSE)
  sprintf("n-%s", y)
}

redecode_namespaced <- redecode_path

display_key <- function(x, config) {
  if (is_encoded_path(x)) {
    sprintf("file %s", decode_path(x = x, config = config))
  } else if (is_encoded_namespaced(x)) {
    sprintf("%s", decode_namespaced(x = x, config = config))
  } else {
    x
  }
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
  encode_path(x)
}
