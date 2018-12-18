analyze_code <- function(
  expr,
  exclude = character(0),
  allowed_globals = NULL,
  as_list = TRUE
) {
  if (!is.function(expr) && !is.language(expr)) {
    return(list())
  }
  results <- new_code_analysis_results()
  locals <- ht_new(c(exclude, ignored_symbols))
  allowed_globals <- ht_new(allowed_globals) %||% NULL
  walk_code(expr, results, locals, allowed_globals)
  if (as_list) {
    results <- list_code_analysis_results(results)
    results <- select_nonempty(results)
  }
  results
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
    ht_add(results$globals, x)
  }
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
    ht_add(results$namespaced, x)
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

analyze_loadd <- function(expr, results) {
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
  out <- setdiff(out, drake_symbols)
  ht_add(results$loadd, out)
}

analyze_readd <- function(expr, results) {
  expr <- match.call(drake::readd, as.call(expr))
  deps <- unlist(analyze_code(expr["target"])[c("globals", "strings")])
  out <- setdiff(deps, drake_symbols)
  ht_add(results$readd, out)
}

analyze_file_in <- function(expr, results) {
  out <- analyze_code(expr[-1], as_list = FALSE)
  files <- drake_quotes(ht_list(out$strings), single = FALSE)
  ht_add(results$file_in, files)
}

analyze_file_out <- function(expr, results) {
  out <- analyze_code(expr[-1], as_list = FALSE)
  files <- drake_quotes(ht_list(out$strings), single = FALSE)
  ht_add(results$file_out, files)
}

analyze_knitr_in <- function(expr, results) {
  files <- analyze_code(expr[-1])$strings
  out <- lapply(files, knitr_deps_list)
  out <- Reduce(out, f = merge_lists)
  files <- drake_quotes(files, single = FALSE)
  ht_add(results$knitr_in, c(out$knitr_in, files))
  lapply(
    X = names(out),
    FUN = function(x) {
      ht_add(results[[x]], out[[x]])
    }
  )
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
    if (nzchar(expr)) {
      ht_add(results$strings, expr)
    }
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
      analyze_loadd(expr, results)
    } else if (name %in% readd_fns) {
      analyze_readd(expr, results)
    } else if (name %in% file_in_fns) {
      analyze_file_in(expr, results)
    } else if (name %in% file_out_fns) {
      analyze_file_out(expr, results)
    } else if (name %in% c(knitr_in_fns)) {
      analyze_knitr_in(expr, results)
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

code_analysis_slots <- c(
  "globals",
  "namespaced",
  "strings",
  "loadd",
  "readd",
  "file_in",
  "file_out",
  "knitr_in"
)

new_code_analysis_results <- function() {
  x <- lapply(
    X = code_analysis_slots,
    FUN = function(tmp) {
      ht_new()
    }
  )
  names(x) <- code_analysis_slots
  list2env(x = x, hash = TRUE, parent = emptyenv())
}

list_code_analysis_results <- function(results) {
  x <- lapply(
    X = code_analysis_slots,
    FUN = function(slot) {
      ht_list(results[[slot]])
    }
  )
  names(x) <- code_analysis_slots
  x
}
