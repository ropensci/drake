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

display_keys <- function(x, config = NULL) {
  vapply(
    X = x,
    FUN = display_key,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE,
    config = config
  )
}

display_key <- function(x, config) {
  if (is_encoded_path(x)) {
    display_path(x = x, config = config)
  } else if (is_encoded_namespaced(x)) {
    sprintf("%s", decode_namespaced(x = x, config = config))
  } else {
    x
  }
}

display_path <- function(x, config) {
  path <- decode_path(x = x, config = config)
  if (is_url(path)) {
    sprintf("url %s", path)
  } else {
    sprintf("file %s", path)
  }
}

standardize_key <- function(text) {
  if (any(grepl("::", text))) {
    text <- encode_namespaced(text)
  }
  text
}

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

is_encoded_namespaced <- function(x) {
  substr(x = x, start = 1, stop = 2) == "n-"
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
