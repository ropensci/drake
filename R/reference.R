#' @export
ref_file <- function(...) {
  path <- as.character(c(...))
  hash <- rehash_local(path, digest_config)
  fields <- list(path = path, hash = hash)
  class <- c("drake_reference_file", "drake_reference")
  vctrs::new_rcrd(fields = fields, class = class)
}

#' @export
format.drake_reference_file <- function(x, ...) {
  vctrs::field(x, "path")
}

#' @export
path <- function(x) {
  UseMethod("path")
}

#' @export
path.drake_reference_file <- function(x) {
  vctrs::field(x, "path")
}

digest_config <- list(cache = list(digest = new_digest_function("xxhash64")))
