#' @export
#' @examples
#' files <- c(tempfile(), tempfile())
#' writeLines("1", files[1])
#' writeLines("2", files[2])
#' ref <- ref_file(files)
ref_file <- function(...) {
  path <- vctrs::vec_c(...)
  hash <- rehash_local(path, digest_config)
  new_ref_file(path = path, hash = hash)
}

new_ref_file <- function(path = character(0), hash = character(0)) {
  vctrs::vec_assert(path, ptype = character())
  vctrs::vec_assert(hash, ptype = character())
  fields <- list(path = path, hash = hash)
  class <- c("drake_reference_file", "drake_reference")
  vctrs::new_rcrd(fields = fields, class = class)
}

#' @export
format.drake_reference_file <- function(x, ...) {
  vctrs::field(x, "path")
}

# Just sketches of methods to allow coercion and casting.
# TODO: clean up.
vec_ptype2.drake_reference_file <- function(x, y, ...) UseMethod("vec_ptype2.drake_reference_file", y)
vec_ptype2.drake_reference_file.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}
vec_ptype2.drake_reference_file.drake_reference_file <- function(x, y, ...) new_ref_file()
vec_ptype2.drake_reference_file.character <- function(x, y, ...) new_ref_file()
vec_ptype2.character.drake_reference_file <- function(x, y, ...) new_ref_file()
vec_cast.drake_reference_file <- function(x, to, ...) UseMethod("vec_cast.drake_reference_file")
vec_cast.drake_reference_file.default <- function(x, to, ...) vec_default_cast(x, to)
vec_cast.drake_reference_file.drake_reference_file <- function(x, to, ...) x
vec_cast.character.drake_reference_file <- function(x, to, ...) field(x, "path")
vec_cast.drake_reference_file.character <- function(x, to, ...) ref_file(x)

digest_config <- list(cache = list(digest = new_digest_function("xxhash64")))
