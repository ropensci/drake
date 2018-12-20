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
  locals <- ht_new_from_list(ignored_symbols_list)
  ht_set(locals, exclude)
  allowed_globals <- ht_new(allowed_globals) %||% NULL
  walk_code(expr, results, locals, allowed_globals)
  if (as_list) {
    results <- list_code_analysis_results(results)
    results <- select_nonempty(results)
  }
  results
}

analyze_strings <- function(expr) {
  ht <- ht_new()
  walk_strings(expr, ht)
  ht_list(ht)
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
    ht_set(results$globals, x)
  }
}

analyze_arrow <- function(expr, results, locals, allowed_globals) {
  walk_call(flatten_assignment(expr[[2]]), results, locals, allowed_globals)
  walk_code(expr[[3]], results, locals, allowed_globals)
  ht_set(locals, get_assigned_var(expr))
}

analyze_for <- function(expr, results, locals, allowed_globals) {
  ht_set(locals, as.character(expr[[2]]))
  walk_call(expr[-2], results, locals, allowed_globals)
}

analyze_function <- function(expr, results, locals, allowed_globals) {
  expr <- unwrap_function(expr)
  if (typeof(expr) != "closure") {
    return()
  }
  locals <- ht_clone(locals)
  ht_set(locals, names(formals(expr)))
  walk_code(formals(expr), results, locals, allowed_globals)
  walk_code(body(expr), results, locals, allowed_globals)
}

analyze_namespaced <- function(expr, results, locals, allowed_globals) {
  x <- wide_deparse(expr)
  if (!ht_exists(locals, x)) {
    ht_set(results$namespaced, x)
  }
}

analyze_assign <- function(expr, results, locals, allowed_globals) {
  expr <- match.call(definition = assign, call = expr)
  if (is.character(expr$x)) {
    ht_set(results$strings, expr$x)
    if (is.null(expr$pos) || identical(expr$pos, formals(assign)$pos)) {
      ht_set(locals, expr$x)
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
  dots <- expr[which_unnamed(expr)]
  ht_set(results$loadd, analyze_strings(expr["list"]))
  ht_set(results$loadd, analyze_strings(dots))
  ht_set(results$loadd, safe_all_vars(dots))
}

analyze_readd <- function(expr, results, allowed_globals) {
  expr <- match.call(drake::readd, as.call(expr))
  ht_set(results$readd, analyze_strings(expr["target"]))
  ht_set(results$readd, safe_all_vars(expr["target"]))
}

analyze_file_in <- function(expr, results) {
  ht_set(results$file_in, analyze_strings(expr[-1]))
}

analyze_file_out <- function(expr, results) {
  ht_set(results$file_out, analyze_strings(expr[-1]))
}

analyze_knitr_in <- function(expr, results) {
  files <- analyze_strings(expr[-1])
  lapply(files, analyze_knitr_file, results = results)
  ht_set(results$knitr_in, files)
}

analyze_knitr_file <- function(file, results) {
  if (!length(file)) {
    return(list())
  }
  fragments <- safe_get_tangled_frags(file)
  out <- analyze_code(fragments, as_list = FALSE)
  if (length(out)){
    for (slot in knitr_in_slots) {
      ht_merge(results[[slot]], out[[slot]])
    }
  }
}

walk_code <- function(expr, results, locals, allowed_globals) {
  if (!length(expr)) {
    return()
  } else if (is.function(expr)) {
    analyze_function(expr, results, locals, allowed_globals)
  } else if (is.name(expr)) {
    analyze_global(expr, results, locals, allowed_globals)
  } else if (is.character(expr)) {
    if (nzchar(expr)) {
      ht_set(results$strings, expr)
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

walk_strings <- function(expr, ht) {
  if (!length(expr)) {
    return()
  } else if (is.function(expr)) {
    walk_strings(formals(expr), ht)
    walk_strings(body(expr), ht)
  } else if (is.character(expr)) {
    if (nzchar(expr)) {
      ht_set(ht, expr)
    }
  } else if (is.pairlist(expr) || is_callish(expr)) {
    lapply(expr, walk_strings, ht = ht)
  }
}
