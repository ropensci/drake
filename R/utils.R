# From lintr
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) <= 0) {
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

braces <- function(x) {
  paste("{\n", x, "\n}")
}

clean_dependency_list <- function(x) {
  if (!length(x)) {
    return(character(0))
  }
  x <- unlist(x)
  x <- unname(x)
  x <- as.character(x)
  x <- unique(x)
  sort(x)
}

exists_tidyselect <- function() {
  suppressWarnings(
    eval(parse(text = "require('tidyselect', quietly = TRUE)"))
  )
}

# Simple version of purrr::pmap for use in drake
# Applies function .f to list .l elements in parallel, i.e.
# .f(.l[[1]][1], .l[[2]][1], ..., .l[[n]][1]) and then
# .f(.l[[1]][2], .l[[2]][2], ..., .l[[n]][2]) etc.
drake_pmap <- function(.l, .f, jobs = 1, ...) {
  stopifnot(is.list(.l))
  stopifnot(is.function(.f))
  stopifnot(is.numeric(jobs))

  if (length(.l) == 0) {
    return(list())  # empty input
  }

  # Ensure identically-lengthed sublists in .l
  len <- unique(unlist(lapply(.l, length)))
  stopifnot(length(len) == 1)

  lightly_parallelize(
    X = seq_len(len),
    FUN = function(i) {
      # extract ith element in each sublist, and then pass to .f
      listi <- lapply(.l, function(x) x[[i]])
      do.call(.f, args = c(listi, ...), quote = TRUE)
    },
    jobs = jobs)
}

drake_tidyselect <- function(
  cache,
  ...,
  namespaces = cache$default_namespace,
  list = character(0)
) {
  tryCatch(
    drake_tidyselect_attempt(
      cache = cache, ..., namespaces = namespaces, list = list
    ),
    error = function(e){
      # nocov start
      eval(parse(text = "require(tidyselect)"))
      drake_tidyselect_attempt(
        cache = cache, ..., namespaces = namespaces, list = list
      )
      # nocov end
    }
  )
}

drake_tidyselect_attempt <- function(
  cache,
  ...,
  namespaces = cache$default_namespace,
  list = character(0)
) {
  out <- tidyselect::vars_select(
    .vars = list_multiple_namespaces(cache = cache, namespaces = namespaces),
    ...,
    .strict = FALSE
  )
  out <- unname(out)
  c(out, list)
}

factor_to_character <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  x
}

file_extn <- function(x) {
  x <- basename(x)
  x <- strsplit(x, split = ".", fixed = TRUE)
  x <- unlist(x)
  x <- rev(x)
  x[1]
}

is_image_filename <- function(x) {
  tolower(file_extn(x)) %in% c("jpg", "jpeg", "pdf", "png")
}

is_imported <- function(target, config) {
  config$layout[[target]]$imported %||% TRUE
}

map_by <- function(.x, .by, .f, ...) {
  splits <- split_by(.x, .by = .by)
  out <- lapply(
    X = splits,
    FUN = function(split){
      out <- .f(split, ...)
      if (nrow(out)) {
        out[, .by] <- split[replicate(nrow(out), 1), .by]
      }
      out
    }
  )
  do.call(what = rbind, args = out)
}

merge_lists <- function(x, y) {
  names <- base::union(names(x), names(y))
  x <- lapply(
    X = names,
    function(name) {
      base::union(x[[name]], y[[name]])
    }
  )
  names(x) <- names
  x
}

padded_scale <- function(x) {
  r <- range(x)
  pad <- 0.2 * (r[2] - r[1])
  c(r[1] - pad, r[2] + pad)
}

random_tempdir <- function() {
  while (file.exists(dir <- tempfile())) {
    Sys.sleep(1e-6) # nocov
  }
  dir.create(dir)
  dir
}

rehash_file_size_cutoff <- 1e5

safe_grepl <- function(pattern, x, ...) {
  tryCatch(grepl(pattern, x, ...), error = error_false)
}

safe_is_na <- function(x) {
  tryCatch(is.na(x), error = error_false, warning = error_false)
}

select_nonempty <- function(x) {
  index <- vapply(
    X = x,
    FUN = function(y) {
      length(y) > 0
    },
    FUN.VALUE = logical(1)
  )
  x[index]
}

select_valid <- function(x) {
  index <- vapply(
    X = x,
    FUN = function(y) {
      length(y) > 0 && !is.na(y)
    },
    FUN.VALUE = logical(1)
  )
  x[index]
}

split_by <- function(.x, .by = character(0)) {
  if (!length(.by)) {
    return(list(.x))
  }
  fact <- lapply(.x[, .by, drop = FALSE], factor, exclude = c())
  splits <- split(x = .x, f = fact)
  Filter(x = splits, f = nrow)
}

standardize_key <- function(text) {
  if (any(grepl("::", text))) {
    text <- encode_namespaced(text)
  }
  # TO DO: remove the rest in version 7.0.0.
  index <- is_quoted(text)
  if (any(index)) {
    text[index] <- gsub("^'|^\"|'$|\"", "", text[index])
    text[index] <- reencode_path(text[index])
  }
  text
}

is_vectorized <- function(funct) {
  if (!is.function(funct)) {
    return(FALSE)
  }
  if (!is.environment(environment(funct))) {
    return(FALSE)
  }
  vectorized_names <- "FUN" # Chose not to include other names.
  if (!all(vectorized_names %in% ls(environment(funct)))) {
    return(FALSE)
  }
  f <- environment(funct)[["FUN"]]
  is.function(f)
}

unwrap_function <- function(funct) {
  if (is_vectorized(funct)) {
    funct <- environment(funct)[["FUN"]]
  }
  funct
}

lock_environment <- function(envir) {
  lockEnvironment(envir, bindings = FALSE)
  lock_these <- ls(envir, all.names = FALSE)
  skip_these <- ".Random.seed"
  lock_these <- setdiff(lock_these, skip_these)
  lapply(X = lock_these, FUN = lockBinding, env = envir)
  invisible()
}

unlock_environment <- function(envir) {
  if (is.null(envir)) {
    stop("use of NULL environment is defunct")
  }
  if (!inherits(envir, "environment")) {
    stop("not an environment")
  }
  .Call("unlock_environment", envir)
  lapply(
    X = ls(envir, all.names = FALSE),
    FUN = unlockBinding,
    env = envir
  )
  stopifnot(!environmentIsLocked(envir))
}

which_unnamed <- function(x) {
  if (!length(names(x))) {
    rep(TRUE, length(x))
  } else {
    !nzchar(names(x))
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

wide_deparse <- function(x) {
  paste(deparse(x), collapse = "\n")
}
