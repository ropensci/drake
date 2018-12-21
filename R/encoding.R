decode_namespaced <- function(x) {
  gsub(pattern = "--", replacement = "::", x = x, fixed = TRUE)
}

decoded_path <- function(x, config) {
  out <- lapply(
    X = x,
    FUN = function(x) {
      config$decode[[x]]
    }
  )
  unlist(out)
}

encode_namespaced <- function(x) {
  gsub(pattern = "::", replacement = "--", x = x, fixed = TRUE)
}

displayed_path <- function(x, config) {
  index <- is_encoded_path(x)
  pretty <- sprintf("file %s", decoded_path(x[index], config))
  c(x[!index], pretty)
}

displayed_path_vector <- function(x, config) {
  vapply(
    X = x,
    FUN = displayed_path,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE,
    config = config
  )
}

is_encoded_namespaced <- function(x) {
  grepl(pattern = "--", x = x, fixed = TRUE)
}

is_encoded_path <- function(x) {
  substr(x = x, start = 0, stop = 1) == "-"
}

not_encoded_path <- function(x) {
  !is_encoded_path(x)
}

redecode_path <- function(x) {
  base64url::base64_urldecode(substr(x, start = 0, stop = nchar(x) - 1))
}

reencode_path <- function(x) {
  paste0(base64url::base64_urlencode(x), "-")
}

redisplay_path <- function(x) {
  index <- is_encoded_path(x)
  pretty <- sprintf("file %s", redecode_path(x[index]))
  c(x[!index], pretty)
}

redisplay_keys <- function(x) {
  vapply(
    X = decode_namespaced(x),
    FUN = redisplay_path,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
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
  reencode_path(x)
}
