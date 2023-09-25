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
  if (is.null(x) || is.na(x)) {
    y
  } else {
    x
  }
}

`%|||NA%` <- function(x, y) {
  if (anyNA(x)) {
    y
  } else {
    x
  }
}

ternary <- function(condition, value_true, value_false) {
  if (any(condition)) {
    value_true
  } else {
    value_false
  }
}

stop0 <- function(...) {
  stop(..., call. = FALSE)
}

warn0 <- function(...) {
  warning(..., call. = FALSE)
}

error_false <- function(e) {
  FALSE
}

error_na <- function(e) {
  NA_character_
}

assert_pkg <- function(pkg, version = NULL, install = "install.packages") {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop0(
      "package ", pkg, " not installed. Install with ",
      install, "(\"", pkg, "\")."
    )
  }
  if (is.null(version)) {
    return()
  }
  installed_version <- as.character(utils::packageVersion(pkg))
  is_too_old <- utils::compareVersion(installed_version, version) < 0
  if (is_too_old) {
    stop0(
      "package ", pkg, " must be version ", version, " or greater. ",
      "Found version ", version, " installed.",
      "Update it with ", install, "(\"", pkg, "\")."
    )
  }
  invisible()
}

assert_static <- function(target, config, function_name) {
  if (is_dynamic(target, config)) {
    stop0(
      "function ",
      function_name,
      " does not support dynamic targets."
    )
  }
}

vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = character(1), ...)
}

viapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = integer(1), ...)
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, FUN.VALUE = logical(1), ...)
}

complete_cases <- function(x) {
  !as.logical(Reduce(`|`, lapply(x, is.na)))
}

safe_vec_c <- function(...) {
  tryCatch(
    vctrs::vec_c(...),
    vctrs_error_scalar_type = function(e) {
      list(...)
    },
    error = function(e) {
      stop(e)
    }
  )
}

safe_is_na <- function(x) {
  tryCatch(is.na(x), error = error_false, warning = error_false)
}

select_nonempty <- function(x) {
  class <- class(x)
  keep <- vapply(
    X = x,
    FUN = function(y) {
      length(y) > 0
    },
    FUN.VALUE = logical(1)
  )
  out <- x[keep]
  if (!is.null(out)) {
    class(out) <- class
  }
  out
}

multiline_message <- function(x, indent = "  ") {
  n <- 30
  if (length(x) > n) {
    x <- c(x[1:(n - 1)], "...")
  }
  x <- paste0(indent, x)
  paste(x, collapse = "\n")
}

min_str <- function(x) {
  n_spaces <- nchar(names(x))
  n_spaces <- max(n_spaces) - n_spaces
  for (index in seq_along(x)) {
    name <- names(x)[index]
    spaces <- paste(rep(" ", n_spaces[index]), collapse = "")
    class <- paste0(class(x[[name]]), collapse = " ")
    cat(" $ ", name, ": ", spaces, class, "\n", sep = "")
  }
}

str0 <- function(x) {
  utils::str(x, give.attr = FALSE, no.list = TRUE)
}

hard_wrap <- Vectorize(
  function(x, width = 0.9 * getOption("width")) {
    if (nchar(x) <= width) {
      return(x)
    }
    chars <- strsplit(x, "")[[1]]
    max_index <- ceiling(nchar(x) / width)
    index <- rep(seq_len(max_index), each = width, length.out = nchar(x))
    lines <- tapply(X = chars, INDEX = index, FUN = paste, collapse = "")
    paste(lines, collapse = "\n")
  },
  vectorize.args = "x",
  USE.NAMES = FALSE
)

soft_wrap <- Vectorize(
  function(x, width = 0.9 * getOption("width")) {
    x <- paste(strwrap(x), collapse = "\n")
    unname(x)
  },
  vectorize.args = "x",
  USE.NAMES = FALSE
)

storage_move <- function(
  from,
  to,
  overwrite = FALSE,
  merge = FALSE,
  warn = TRUE,
  jobs = 1L
) {
  if (dir.exists(from)) {
    dir_move(
      from = from,
      to = to,
      overwrite = overwrite,
      merge = merge,
      warn = warn,
      jobs = jobs
    )
  } else {
    file_move(from = from, to = to)
  }
  invisible()
}

dir_move <- function(
  from,
  to,
  overwrite = FALSE,
  merge = FALSE,
  warn = TRUE,
  jobs = 1L
) {
  if (!overwrite && file.exists(to)) {
    if (warn) {
      warn0(
        "cannot move ", from, " to ", to, ". ",
        to, " already exists."
      )
    }
    return(invisible())
  }
  if (!merge) {
    unlink(to, recursive = TRUE)
  }
  dir_create(to)
  files <- list.files(from, all.files = TRUE, recursive = TRUE)
  args <- list(
    from = file.path(from, files),
    to = file.path(to, files)
  )
  drake_pmap(.l = args, .f = file_move, jobs = jobs)
  unlink(from, recursive = TRUE)
  invisible()
}

file_move <- function(from, to) {
  dir_create(dirname(to))
  file.rename(from = from, to = to)
  invisible()
}

storage_copy <- function(
  from,
  to,
  overwrite = FALSE,
  merge = FALSE,
  warn = TRUE,
  jobs = 1L
) {
  if (dir.exists(from)) {
    dir_copy(
      from = from,
      to = to,
      overwrite = overwrite,
      merge = merge,
      warn = warn,
      jobs = jobs
    )
  } else {
    file_copy(from = from, to = to, overwrite = overwrite)
  }
  invisible()
}

dir_copy <- function(
  from,
  to,
  overwrite = FALSE,
  merge = FALSE,
  warn = TRUE,
  jobs = 1L
) {
  if (!overwrite && file.exists(to)) {
    if (warn) {
      warn0(
        "cannot move ", from, " to ", to, ". ",
        to, " already exists."
      )
    }
    return(invisible())
  }
  if (!merge) {
    unlink(to, recursive = TRUE)
  }
  dir_create(to)
  files <- list.files(from, all.files = TRUE, recursive = TRUE)
  args <- list(
    from = file.path(from, files),
    to = file.path(to, files)
  )
  drake_pmap(.l = args, .f = file_copy, overwrite = overwrite, jobs = jobs)
  invisible()
}

file_copy <- function(from, to, overwrite = FALSE) {
  dir_create(dirname(to))
  file.copy(from = from, to = to, overwrite = overwrite)
  invisible()
}

file_remove <- function(file) {
  if (file.exists(file)) {
    unlink(file, recursive = TRUE)
  }
}

dir_create <- function(x) {
  if (!file.exists(x)) {
    dir.create(x, showWarnings = FALSE, recursive = TRUE)
  }
  if (!dir.exists(x)) {
    stop0("cannot create directory at ", x)
  }
  invisible()
}

weak_as_tibble <- function(..., .force_df = FALSE) {
  no_tibble <- !suppressWarnings(requireNamespace("tibble", quietly = TRUE))
  if (.force_df || no_tibble) {
    as.data.frame(..., stringsAsFactors = FALSE)
  } else {
    tibble::as_tibble(...)
  }
}

weak_tibble <- function(..., .force_df = FALSE) {
  no_tibble <- !suppressWarnings(requireNamespace("tibble", quietly = TRUE))
  if (.force_df || no_tibble) {
    data.frame(..., stringsAsFactors = FALSE)
  } else {
    tibble::tibble(...)
  }
}

#' @title Isolate the side effects of an example.
#' `r lifecycle::badge("stable")`
#' @description Runs code in a temporary directory
#'   in a controlled environment with a controlled
#'   set of options.
#' @export
#' @keywords internal
#' @return Nothing.
#' @param desc Character, description of the example.
#' @param code Code to run.
isolate_example <- function(desc, code) {
  new <- tempfile()
  dir.create(new)
  old <- setwd(new) # nolint
  on.exit(setwd(old)) # nolint
  opts <- list(drake_clean_menu = FALSE)
  with_options(new = opts, code)
  invisible()
}
