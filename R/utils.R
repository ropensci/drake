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

assert_cache <- function(cache) {
  if (is.null(cache)) {
    stop("cannot find drake cache.", call. = FALSE)
  }
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

error_false <- function(e) {
  FALSE
}

error_na <- function(e) {
  NA_character_
}

all_is_na <- function(x) {
  all(is.na(x))
}

safe_is_na <- function(x) {
  tryCatch(is.na(x), error = error_false, warning = error_false)
}

complete_cases <- function(x) {
  !as.logical(Reduce(`|`, lapply(x, is.na)))
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

num_unique <- function(x) {
  length(unique(x))
}

set_names <- function(x, nms) {
  names(x) <- nms
  x
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

multiline_message <- function(x) {
  n <- 30
  if (length(x) > n) {
    x <- c(x[1:(n - 1)], "...")
  }
  x <- paste0("  ", x)
  paste(x, collapse = "\n")
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
      warning(
        "cannot move ", from, " to ", to, ". ",
        to, " already exists.",
        call. = FALSE
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

dir_create <- function(x) {
  if (!file.exists(x)) {
    dir.create(x, showWarnings = FALSE, recursive = TRUE)
  }
  if (!dir.exists(x)) {
    stop("cannot create directory at ", shQuote(x), call. = FALSE)
  }
  invisible()
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
