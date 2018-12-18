analyze_code <- function(
  expr,
  exclude = character(0),
  allowed_globals = NULL
) {
  if (!is.function(expr) && !is.language(expr)) {
    return(list())
  }
  results <- ht_new()
  locals <- ht_new(c(exclude, ignored_symbols))
  allowed_globals <- ht_new(allowed_globals) %||% NULL
  walk_code(expr, results, locals, allowed_globals)
  results <- lapply(as.list(results), unique)
  select_nonempty(results)
}

analyze_global <- function(expr, results, locals, allowed_globals) {
  x <- as.character(expr)
  if (!nzchar(x)) {
    return()
  }
  if (ht_exists(locals, x)) {
    return()
  }
  if (is.null(allowed_globals) || ht_exists(allowed_globals, x)) {
    results$globals <- c(results$globals, x)
  }
  invisible()
}

analyze_arrow <- function(expr, results, locals, allowed_globals) {
  walk_call(flatten_assignment(expr[[2]]), results, locals, allowed_globals)
  walk_code(expr[[3]], results, locals, allowed_globals)
  ht_add(locals, get_assigned_var(expr))
}

analyze_for <- function(expr, results, locals, allowed_globals) {
  ht_add(locals, as.character(expr[[2]]))
  walk_call(expr[-2], results, locals, allowed_globals)
}

analyze_function <- function(expr, results, locals, allowed_globals) {
  expr <- unwrap_function(expr)
  if (typeof(expr) != "closure") {
    return()
  }
  locals <- ht_clone(locals)
  ht_add(locals, names(formals(expr)))
  walk_code(formals(expr), results, locals, allowed_globals)
  walk_code(body(expr), results, locals, allowed_globals)
}

analyze_namespaced <- function(expr, results, locals, allowed_globals) {
  x <- wide_deparse(expr)
  if (!ht_exists(locals, x)) {
    results$namespaced <- c(results$namespaced, x)
  }
}

analyze_assign <- function(expr, results, locals, allowed_globals) {
  expr <- match.call(definition = assign, call = expr)
  if (is.character(expr$x)) {
    if (is.null(expr$pos) || identical(expr$pos, formals(assign)$pos)) {
      ht_add(locals, expr$x)
    }
  } else {
    analyze_global(expr$x, results, locals, allowed_globals)
  }
  expr$x <- NULL
  walk_call(expr, results, locals, allowed_globals)
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
    analyze_function(expr, results, locals, allowed_globals)
  } else if (is.name(expr)) {
    analyze_global(expr, results, locals, allowed_globals)
  } else if (is.character(expr)) {
    results$strings <- c(results$strings, expr)
  } else if (is.pairlist(expr)) {
    walk_call(expr, results, locals, allowed_globals)
  } else if (is.language(expr) && (is.call(expr) || is.recursive(expr))) {
    name <- wide_deparse(expr[[1]])
    if (name == "local"){
      locals <- ht_clone(locals)
    }
    if (name %in% c("expression", "quote", "Quote")) {
      analyze_global(name, results, locals, allowed_globals)
    } else if (name %in% c("<-", "=")) {
      analyze_arrow(expr, results, locals, allowed_globals)
    } else if (name %in% c("::", ":::")) {
      analyze_namespaced(expr, results, locals, allowed_globals)
    } else if (name == "for") {
      analyze_for(expr, results, locals, allowed_globals)
    } else if (name == "function") {
      analyze_function(eval(expr), results, locals, allowed_globals)
    } else if (name %in% c("assign", "delayedAssign")) {
      analyze_assign(expr, results, locals, allowed_globals)
    } else if (name %in% loadd_fns) {
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
      walk_call(expr, results, locals, allowed_globals)
    }
  }
}

walk_call <- function(expr, results, locals, allowed_globals) {
  lapply(
    X = expr,
    FUN = walk_code,
    results = results,
    locals = locals,
    allowed_globals = allowed_globals
  )
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
