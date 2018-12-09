code_dependencies <- function(expr, exclude = character(0), globals = NULL) {
  if (
    !is.function(expr) &&
    !is.expression(expr) &&
    !is.language(expr)
  ) {
    return(list())
  }
  results <- list()
  # `walk()` analyzes `drake`-specific calls
  # in an expression or function.
  # It sees `results` in its lexical scope.
  walk <- function(expr) {
    if (!length(expr)) {
      return()
    } else if (is.function(expr)) {
      expr <- unwrap_function(expr)
      if (typeof(expr) != "closure") {
        expr <- function() {} # nolint: curly braces are necessary
      }
      walk(body(expr))
    } else if (is.name(expr)) {
      results$globals <<- c(results$globals, expr)
    } else if (is.character(expr)) {
      results$strings <<- c(results$strings, expr)
    } else if (is.language(expr) && (is.call(expr) || is.recursive(expr))) {
      new_results <- list()
      if (is_loadd_call(expr)) {
        new_results <- analyze_loadd(expr)
      } else if (is_readd_call(expr)) {
        new_results <- analyze_readd(expr)
      } else if (is_knitr_in_call(expr)) {
        new_results <- analyze_knitr_in(expr)
      } else if (is_file_in_call(expr)) {
        new_results <- analyze_file_in(expr)
      } else if (is_file_out_call(expr)) {
        new_results <- analyze_file_out(expr)
      } else if (!is_ignored_call(expr)) {
        if (wide_deparse(expr[[1]]) %in% c("::", ":::")) {
          new_results <- list(
            namespaced = setdiff(wide_deparse(expr), drake_symbols)
          )
        } else {
          lapply(X = expr, FUN = walk)
        }
      }
      results <<- zip_lists(x = results, y = new_results)
    }
  }
  walk(expr)
  results <- lapply(results, unique)
  results$globals <- as.character(results$globals)
  non_locals <- find_non_locals(expr)
  results$globals <- intersect(results$globals, non_locals)
  if (!is.null(globals)) {
    results$globals <- intersect(results$globals, globals)
  }
  exclude <- base::union(exclude, ".")
  results <- lapply(
    X = results,
    FUN = function(x) {
      setdiff(x, exclude)
    }
  )
  select_nonempty(results)
}

# Walk through function f and find `pkg::fun()` and `pkg:::fun()` calls.
find_namespaced_functions <- function(f, found = character(0)) {
  if (is.function(f)) {
    return(find_namespaced_functions(body(f), found))
  } else if (is.call(f) && wide_deparse(f[[1]]) %in% c("::", ":::")) {
    found <- c(found, wide_deparse(f))
  } else if (is.recursive(f)) {
    v <- lapply(as.list(f), find_namespaced_functions, found)
    found <- unique(c(found, unlist(v)))
  }
  found
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
      fun <- eval(parse(text = rlang::expr_text(fun))) # nocov
      codetools::findGlobals(fun = fun, merge = TRUE)  # nocov
    }
  )
  setdiff(out, ignored_symbols)
}

analyze_loadd <- function(expr) {
  expr <- match.call(drake::loadd, as.call(expr))
  expr <- expr[-1]
  unnamed <- list()
  if (any(is_unnamed <- which_unnamed(expr))) {
    unnamed <- code_dependencies(expr[is_unnamed])
  }
  out <- c(
    unnamed$globals,
    unnamed$strings,
    code_dependencies(expr["list"])$strings
  )
  list(loadd = setdiff(out, drake_symbols))
}

analyze_readd <- function(expr) {
  expr <- match.call(drake::readd, as.call(expr))
  deps <- unlist(code_dependencies(expr["target"])[c("globals", "strings")])
  list(readd = setdiff(deps, drake_symbols))
}

analyze_file_in <- function(expr) {
  expr <- expr[-1]
  deps <- drake_quotes(code_dependencies(expr)$strings, single = FALSE)
  list(file_in = deps)
}

analyze_file_out <- function(expr) {
  expr <- expr[-1]
  deps <- drake_quotes(code_dependencies(expr)$strings, single = FALSE)
  list(file_out = deps)
}

analyze_knitr_in <- function(expr) {
  expr <- expr[-1]
  files <- code_dependencies(expr)$strings
  out <- lapply(files, knitr_deps_list)
  out <- Reduce(out, f = merge_lists)
  files <- drake_quotes(files, single = FALSE)
  out$knitr_in <- base::union(out$knitr_in, files)
  out
}

which_unnamed <- function(x) {
  if (!length(names(x))) {
    rep(TRUE, length(x))
  } else {
    !nzchar(names(x))
  }
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
misc_syms <- "."
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
    misc_syms,
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

is_ignored_call <- function(expr) {
  wide_deparse(expr[[1]]) %in% ignored_fns
}

is_file_in_call <- function(expr) {
  wide_deparse(expr[[1]]) %in% file_in_fns
}

is_file_out_call <- function(expr) {
  wide_deparse(expr[[1]]) %in% file_out_fns
}

is_knitr_in_call <- function(expr) {
  wide_deparse(expr[[1]]) %in% c(knitr_in_fns)
}

is_loadd_call <- function(expr) {
  wide_deparse(expr[[1]]) %in% loadd_fns
}

is_readd_call <- function(expr) {
  wide_deparse(expr[[1]]) %in% readd_fns
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
