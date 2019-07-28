# From lintr
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) <= 0) {
    y
  } else {
    x
  }
}

`%|||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

`%||NA%` <- function(x, y) {
  if (is.null(x) || length(x) < 1 || anyNA(x)) {
    y
  } else {
    x
  }
}

all_targets <- function(config) {
  out <- V(config$graph)$name[!V(config$graph)$imported]
  out[!is_encoded_path(out)]
}

all_imports <- function(config) {
  V(config$graph)$name[V(config$graph)$imported]
}

is_imported <- function(target, config) {
  config$layout[[target]]$imported %||% TRUE
}

assert_config_not_plan <- function(config) {
  if (!inherits(config, "drake_plan")) {
    return()
  }
  stop(
    "You supplied a drake plan to the ",
    shQuote("config"),
    " argument of a function. Instead, please call ",
    shQuote("drake_config()"),
    " on the plan and then supply the return value to ",
    shQuote("config"),
    ".",
    call. = FALSE
  )
}

assert_pkg <- function(pkg, version = NULL, install = "install.packages") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "package ", pkg, " not installed. ",
      "Please install it with ", install, "(\"", pkg, "\").",
      call. = FALSE
    )
  }
  if (is.null(version)) {
    return()
  }
  installed_version <- as.character(utils::packageVersion(pkg))
  is_too_old <- utils::compareVersion(installed_version, version) < 0
  if (is_too_old) {
    stop(
      "package ", pkg, " must be version ", version, " or greater. ",
      "Found version ", version, " installed.",
      "Please update it with ", install, "(\"", pkg, "\").",
      call. = FALSE
    )
  }
  invisible()
}

# weak_as_tibble - use as_tibble() if available but fall back to
# as.data.frame() if necessary
weak_as_tibble <- function(..., .force_df = FALSE) {
  no_tibble <- !suppressWarnings(requireNamespace("tibble", quietly = TRUE))
  if (.force_df || no_tibble) {
    as.data.frame(..., stringsAsFactors = FALSE)
  } else {
    tibble::as_tibble(...)
  }
}

# weak_tibble - use tibble() if available but fall back to
# data.frame() if necessary
weak_tibble <- function(..., .force_df = FALSE) {
  no_tibble <- !suppressWarnings(requireNamespace("tibble", quietly = TRUE))
  if (.force_df || no_tibble) {
    data.frame(..., stringsAsFactors = FALSE)
  } else {
    tibble::tibble(...)
  }
}

safe_is_na <- function(x) {
  tryCatch(is.na(x), error = error_false, warning = error_false)
}

error_false <- function(e) {
  FALSE
}

error_na <- function(e) {
  NA_character_
}

select_nonempty <- function(x) {
  keep <- vapply(
    X = x,
    FUN = function(y) {
      length(y) > 0
    },
    FUN.VALUE = logical(1)
  )
  x[keep]
}

multiline_message <- function(x) {
  n <- 30
  if (length(x) > n) {
    x <- c(x[1:(n - 1)], "...")
  }
  x <- paste0("  ", x)
  paste(x, collapse = "\n")
}

# From withr https://github.com/r-lib/withr, copyright RStudio, GPL (>=2)
with_options <- function(new, code) {
  old <- set_options(new_options = new)
  on.exit(set_options(new_options = old))
  force(code)
}

# From withr https://github.com/r-lib/withr, copyright RStudio, GPL (>=2)
set_options <- function(new_options) {
  do.call(options, as.list(new_options))
}
