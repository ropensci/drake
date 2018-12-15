# The analyze_*() functions analyze language objects from the top.
# These functions tend to give sensible answers if you just supply
# the language object to the first argument and rely on the defaults
# of the other arguments.
analyze_code <- function(
  expr,
  exclude = character(0),
  allowed_globals = NULL
) {
  if (!is.function(expr) && !is.language(expr)) {
    return(list())
  }
  locals <- ht_new()
  results <- ht_new()
  ht_add(locals, c(exclude, drake_symbols))
  walk_code(expr, results, locals = locals, allowed_globals = allowed_globals)
  results <- lapply(as.list(results), unique)
  results$globals <- as.character(results$globals)
  non_locals <- find_non_locals(expr)
  results$globals <- intersect(results$globals, non_locals)
  if (!is.null(allowed_globals)) {
    results$globals <- intersect(results$globals, allowed_globals)
  }
  results <- lapply(
    X = results,
    FUN = function(x) {
      unique(setdiff(x, exclude))
    }
  )
  select_nonempty(results)
}

analyze_loadd <- function(expr) {
  expr <- match.call(drake::loadd, as.call(expr))
  expr <- expr[-1]
  unnamed <- list()
  if (any(is_unnamed <- which_unnamed(expr))) {
    unnamed <- analyze_code(expr[is_unnamed])
  }
  out <- c(
    unnamed$globals,
    unnamed$strings,
    analyze_code(expr["list"])$strings
  )
  list(loadd = setdiff(out, drake_symbols))
}

analyze_readd <- function(expr) {
  expr <- match.call(drake::readd, as.call(expr))
  deps <- unlist(analyze_code(expr["target"])[c("globals", "strings")])
  list(readd = setdiff(deps, drake_symbols))
}

analyze_file_in <- function(expr) {
  expr <- expr[-1]
  deps <- drake_quotes(analyze_code(expr)$strings, single = FALSE)
  list(file_in = deps)
}

analyze_file_out <- function(expr) {
  expr <- expr[-1]
  deps <- drake_quotes(analyze_code(expr)$strings, single = FALSE)
  list(file_out = deps)
}

analyze_knitr_in <- function(expr) {
  expr <- expr[-1]
  files <- analyze_code(expr)$strings
  out <- lapply(files, knitr_deps_list)
  out <- Reduce(out, f = merge_lists)
  files <- drake_quotes(files, single = FALSE)
  out$knitr_in <- c(out$knitr_in, files)
  out
}

# The walk_*() functions are repeated recursion steps inside
# the analyze_*() functions. For walk_*(), the secondary arguments
# are important for the recursion to work,
# and no arguments have defaults.
walk_code <- function(expr, results, locals, allowed_globals) {
  if (!length(expr)) {
    return()
  } else if (is.function(expr)) {
    walk_function(expr, results, locals, allowed_globals)
  } else if (is.name(expr)) {
    results$globals <- c(results$globals, expr)
  } else if (is.character(expr)) {
    results$strings <- c(results$strings, expr)
  } else if (is.language(expr) && (is.call(expr) || is.recursive(expr))) {
    name <- wide_deparse(expr[[1]])
    if (name %in% loadd_fns) {
      zip_to_envir(analyze_loadd(expr), results)
    } else if (name %in% readd_fns) {
      zip_to_envir(analyze_readd(expr), results)
    } else if (name %in% c(knitr_in_fns)) {
      zip_to_envir(analyze_knitr_in(expr), results)
    } else if (name %in% file_in_fns) {
      zip_to_envir(analyze_file_in(expr), results)
    } else if (name %in% file_out_fns) {
      zip_to_envir(analyze_file_out(expr), results)
    } else if (!(name %in% ignored_fns)) {
      walk_call(expr, name, results, locals, allowed_globals)
    }
  }
}

walk_call <- function(expr, name, results, locals, allowed_globals) {
  if (name %in% c("::", ":::")) {
    results$namespaced <- c(
      results$namespaced,
      setdiff(wide_deparse(expr), drake_symbols)
    )
  } else {
    lapply(
      X = expr,
      FUN = walk_code,
      results = results,
      locals = locals,
      allowed_globals = allowed_globals
    )
  }
}

walk_function <- function(expr, results, locals, allowed_globals) {
  expr <- unwrap_function(expr)
  if (typeof(expr) == "closure") {
    walk_code(
      expr = body(expr),
      results = results,
      locals = locals,
      allowed_globals = allowed_globals
    )
  }
}

is_target_call <- function(expr) {
  tryCatch(
    wide_deparse(expr[[1]]) %in% target_fns,
    error = error_false
  )
}

is_trigger_call <- function(expr) {
  tryCatch(
    wide_deparse(expr[[1]]) %in% trigger_fns,
    error = error_false
  )
}

is_callish <- function(x) {
  length(x) > 0 && is.language(x) && (is.call(x) || is.recursive(x))
}

pair_text <- function(x, y) {
  apply(expand.grid(x, y), 1, paste0, collapse = "")
}

drake_prefix <- c("", "drake::", "drake:::")
drake_envir_marker <- "._drake_envir"
file_in_fns <- pair_text(drake_prefix, c("file_in"))
file_out_fns <- pair_text(drake_prefix, c("file_out"))
ignored_fns <- pair_text(drake_prefix, c("drake_envir", "ignore"))
knitr_in_fns <- pair_text(drake_prefix, c("knitr_in"))
loadd_fns <- pair_text(drake_prefix, "loadd")
readd_fns <- pair_text(drake_prefix, "readd")
target_fns <- pair_text(drake_prefix, "target")
trigger_fns <- pair_text(drake_prefix, "trigger")

drake_symbols <- sort(
  c(
    drake_envir_marker,
    file_in_fns,
    file_out_fns,
    ignored_fns,
    loadd_fns,
    knitr_in_fns,
    readd_fns,
    target_fns,
    trigger_fns
  )
)
base_symbols <- sort(
  grep(
    pattern = "[a-zA-Z]",
    x = ls("package:base"),
    value = TRUE,
    invert = TRUE
  )
)
ignored_symbols <- sort(c(drake_symbols, base_symbols))

find_non_locals <- function(fun) {
  if (!is.function(fun)) {
    f <- function() {} # nolint
    body(f) <- as.call(append(as.list(body(f)), fun))
    fun <- f
  }
  if (typeof(fun) != "closure") {
    return(character(0))
  }
  fun <- unwrap_function(fun)
  # The tryCatch statement fixes a strange bug in codetools
  # for R 3.3.3. I do not understand it.
  out <- tryCatch(
    codetools::findGlobals(fun = fun, merge = TRUE),
    error = function(e) {
      fun <- eval(parse(text = wide_deparse(fun))) # nocov
      codetools::findGlobals(fun = fun, merge = TRUE)  # nocov
    }
  )
  setdiff(out, ignored_symbols)
}
