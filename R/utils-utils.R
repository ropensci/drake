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

clean_dependency_list <- function(x) {
  sort(clean_nested_char_list(x))
}

clean_nested_char_list <- function(x) {
  if (!length(x)){
    return(character(0))
  }
  x <- unlist(x)
  x <- unname(x)
  x <- as.character(x)
  x <- unique(x)
}

complete_cases <- function(x) {
  !as.logical(Reduce(`|`, lapply(x, is.na)))
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

na_omit <- function(x) {
  x[!is.na(x)]
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

safe_is_na <- function(x) {
  tryCatch(is.na(x), error = error_false, warning = error_false)
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

select_valid_lang <- function(x) {
  discard <- vapply(
    X = x,
    FUN = function(y) {
      identical(y, substitute())
    },
    FUN.VALUE = logical(1)
  )
  x[!discard]
}

select_valid <- function(x) {
  index <- vapply(
    X = x,
    FUN = function(y) {
      length(y) > 0 && !safe_is_na(y)
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
  if (!all(vectorized_names %in% names(environment(funct)))) {
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
  lapply(X = unhidden_names(envir), FUN = lockBinding, env = envir)
  invisible()
}

unlock_environment <- function(envir) {
  if (is.null(envir)) {
    stop("use of NULL environment is defunct")
  }
  if (!inherits(envir, "environment")) {
    stop("not an environment")
  }
  .Call(Cunlock_environment, envir)
  lapply(
    X = unhidden_names(envir),
    FUN = unlockBinding,
    env = envir
  )
  stopifnot(!environmentIsLocked(envir))
}

unhidden_names <- function(envir) {
  out <- names(envir)
  out <- out[substr(out, 0, 1) != "."]
  out
}

is_unnamed <- function(x) {
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

drake_bind_rows <- function(...) {
  args <- select_nonempty(list(...))
  df_env <- new.env(parent = emptyenv())
  df_env$dfs <- list()
  flatten_df_list(args, df_env = df_env)
  dfs <- df_env$dfs
  cols <- lapply(dfs, colnames)
  cols <- Reduce(f = union, x = cols)
  dfs <- lapply(dfs, fill_cols, cols = cols)
  do.call(rbind, dfs)
}

flatten_df_list <- function(args, df_env){
  if (!is.null(dim(args))) {
    index <- length(df_env$dfs) + 1
    df_env$dfs[[index]] <- weak_as_tibble(args)
  } else {
    lapply(args, flatten_df_list, df_env = df_env)
  }
}

fill_cols <- function(x, cols) {
  for (col in setdiff(cols, colnames(x))) {
    x[[col]] <- rep(NA, nrow(x))
  }
  x
}

safe_deparse <- function(x, collapse = "\n") {
  paste(
    deparse(x, control = c("keepInteger", "keepNA")),
    collapse = collapse
  )
}

safe_parse <- function(x) {
  out <- parse(text = x, keep.source = FALSE)
  if (length(out)) {
    out <- out[[1]]
  }
  out
}

named <- function(x, exclude = character(0)) {
  if (is.null(names(x))) return(NULL)
  x[!(names(x) %in% c("", exclude))]
}

unnamed <- function(x) {
  if (is.null(names(x))) return(x)
  x[!nzchar(names(x))]
}

sub_in_plan <- function(plan, rows, index) {
  plan <- drake_bind_rows(
    plan[seq_len(index - 1), ],
    rows,
    plan[-seq_len(index), ]
  )
  plan
}

tidyeval_exprs <- function(expr_list, envir) {
  lapply(expr_list, tidyeval_expr, envir = envir)
}

tidyeval_expr <- function(expr, envir) {
  call <- as.call(c(quote(rlang::expr), expr))
  eval(call, envir = envir)
}

# From https://stackoverflow.com/a/54623901/3704549
splice_inner <- function(x, replacements) {
  if (is.call(x)) {
    as.call(
      do.call(
        "c",
        lapply(as.list(x), splice_inner, replacements),
        quote = TRUE
      )
    )
  } else if (is.name(x)) {
    nm <- deparse(x)
    if (nm %in% names(replacements)) {
      return(replacements[[nm]])
    } else {
      list(x)
    }
  } else {
    list(x)
  }
}

splice_args <- function(x, replacements) {
  out <- splice_inner(x, replacements)
  # Avoid edge cases like #715
  out <- parse(text = safe_deparse(out)) # safe_deparse() is internal to drake.
  if (length(out)) {
    out <- out[[1]]
  }
  out
}

# from base::remove()
targets_from_dots <- function(dots, list) {
  valid <- vapply(
    dots,
    function(x) is.symbol(x) || is.character(x),
    NA,
    USE.NAMES = FALSE
  )
  invalid <- length(dots) && !all(valid)
  if (invalid) {
    stop("... must contain names or character strings", call. = FALSE)
  }
  names <- vapply(dots, as.character, "")
  targets <- unique(c(names, list))
  standardize_key(targets)
}

make_unique <- function(x) {
  if (!length(x)) {
    return(character(0))
  }
  ord <- order(x)
  y <- x[ord]
  dup <- duplicated(y)
  if (!any(dup)) {
    return(x)
  }
  suffix <- as.integer(
    do.call(c, tapply(dup, y, FUN = cumsum, simplify = FALSE))
  )
  i <- suffix > 0L
  suffix <- suffix + i
  y[i] <- paste(y[i], suffix[i], sep = "_")
  y[order(ord)]
}
