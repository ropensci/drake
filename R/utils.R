#' @title Isolate the side effects of an example.
#' \lifecycle{stable}
#' @description Runs code in a temporary directory
#'   in a controlled environment with a controlled
#'   set of options.
#' @export
#' @keywords internal
#' @return Nothing.
#' @param desc Character, description of the example.
#' @param ... Code to run.
isolate_example <- function(desc, code) {
  new <- tempfile()
  dir.create(new)
  old <- setwd(new) # nolint
  on.exit(setwd(old)) # nolint
  opts <- list(drake_clean_menu = FALSE)
  with_options(new = opts, code)
  invisible()
}

all_targets <- function(config) {
  out <- V(config$graph)$name[!V(config$graph)$imported]
  out[!is_encoded_path(out)]
}

all_imports <- function(config) {
  V(config$graph)$name[V(config$graph)$imported]
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

longest_match <- function(choices, against) {
  index <- vapply(
    choices,
    pmatch,
    table = against,
    FUN.VALUE = integer(1)
  )
  matches <- names(index[!is.na(index)])
  matches[which.max(nchar(matches))]
}

random_string <- function(exclude) {
  key <- NULL
  while (is.null(key) || (key %in% exclude)) {
    key <- basename(tempfile())
  }
  key
}

random_tempdir <- function() {
  while (file.exists(dir <- tempfile())) {
    Sys.sleep(1e-6) # nocov
  }
  dir.create(dir)
  dir
}

all_is_na <- function(x) {
  all(is.na(x))
}

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
