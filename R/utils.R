# From lintr
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) <= 0) {
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

is_file <- function(x) {
  x <- substr(x = x, start = 0, stop = 1)
  x == "\"" | x == "'" # TODO: get rid fo the single quote next major release
}

is_image_filename <- function(x) {
  tolower(file_extn(x)) %in% c("jpg", "jpeg", "pdf", "png")
}

is_imported <- function(target, config) {
  config$layout[[target]]$imported %||% TRUE
}

is_not_file <- function(x) {
  !is_file(x)
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
  f <- lapply(.x[, .by], factor, exclude = c())
  splits <- split(x = .x, f = f)
  Filter(x = splits, f = nrow)
}

standardize_filename <- function(text) {
  text[is_file(text)] <-  gsub("^'|'$", "\"", text[is_file(text)])
  text
}

zip_lists <- function(x, y) {
  names <- base::union(names(x), names(y))
  x <- lapply(
    X = names,
    function(name) {
      c(x[[name]], y[[name]])
    }
  )
  names(x) <- names
  x
}

# Simple version of purrr::pmap for use in drake
# Applies function .f to list .l elements in parallel, i.e.
# .f(.l[[1]][1], .l[[2]][1], ..., .l[[n]][1]) and then
# .f(.l[[1]][2], .l[[2]][2], ..., .l[[n]][2]) etc.
drake_pmap <- function(.l, .f, ...) {
  stopifnot(is.list(.l))
  stopifnot(is.function(.f))
  if(length(.l) == 0) return(list()) # empty input
  
  # Ensure identically-lengthed sublists in .l
  len <- unique(unlist(lapply(.l, length)))
  stopifnot(length(len) == 1)

  out <- list()
  for (i in seq_len(len)) {
    # extract ith element in each sublist
    listi <- lapply(.l, function(x) x[[i]])
    out[[i]] <- do.call(.f, args = c(listi, ...), quote = TRUE)
  }
  out
}
