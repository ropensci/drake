#' @title Static code analysis
#' @keywords internal
#' @description Static code analysis.
#' @param expr A function or expression.
#' @param results A `drake_deps` object.
#' @param locals An environment, a hash table of local variables.
#' @param restrict An environment,
#'   a hash table for whitelisting global symbols.
walk_code <- function(expr, results, locals, restrict) {
  tryCatch(
    try_walk_code(expr, results, locals, restrict),
    error = function(e) {
      stop(
        paste0(conditionMessage(e), collapse = " "),
        "\nMalformed code:\n",
        safe_deparse(expr, collapse = " "),
        "\n",
        call. = FALSE
      )
    }
  )
}

try_walk_code <- function(expr, results, locals, restrict) {
  if (!length(expr)) {
    return()
  } else if (is.function(expr)) {
    analyze_function(expr, results, locals, restrict)
  } else if (is.name(expr)) {
    analyze_global(expr, results, locals, restrict)
  } else if (is.character(expr)) {
    str <- expr[nzchar(expr)]
    for (x in str) {
      ht_set(results$strings, x)
    }
  } else if (is.pairlist(expr)) {
    walk_recursive(expr, results, locals, restrict)
  } else if (is.call(expr) || is.recursive(expr)) {
    walk_call(expr, results, locals, restrict)
  }
  invisible()
}

walk_call <- function(expr, results, locals, restrict) { # nolint
  name <- safe_deparse(expr[[1]], backtick = FALSE)
  if (name == "local") {
    locals <- ht_clone(locals)
  }
  if (name %in% c("$", "@")) {
    expr[[3]] <- substitute()
  }
  if (walk_base(expr, results, locals, restrict, name)) {
    return()
  }
  walk_drake(expr, results, locals, restrict, name)
}

walk_base <- function(expr, results, locals, restrict, name) {
  out <- TRUE
  if (name %in% c("expression", "quote", "Quote")) {
    analyze_global(name, results, locals, restrict)
  } else if (name %in% c("<-", "=")) {
    analyze_arrow(expr, results, locals, restrict)
  } else if (name %in% c("::", ":::")) {
    analyze_namespaced(expr, results, locals, restrict)
  } else if (name == "for") {
    analyze_for(expr, results, locals, restrict)
  } else if (name == "function") {
    analyze_function(eval(expr), results, locals, restrict)
  } else if (name == "assign") {
    analyze_assign(expr, results, locals, restrict)
  } else if (name == "delayedAssign") {
    analyze_delayed_assign(expr, results, locals, restrict)
  } else if (name == "UseMethod") {
    analyze_usemethod(expr, results, locals, restrict)
  } else {
    out <- FALSE
  }
  out
}

walk_drake <- function(expr, results, locals, restrict, name) {
  if (name %in% loadd_fns) {
    analyze_loadd(expr, results, restrict)
  } else if (name %in% readd_fns) {
    analyze_readd(expr, results, restrict)
  } else if (name %in% file_in_fns) {
    analyze_file_in(expr, results)
  } else if (name %in% file_out_fns) {
    analyze_file_out(expr, results)
  } else if (name %in% c(knitr_in_fns)) {
    analyze_knitr_in(expr, results, restrict)
  } else if (!(name %in% no_deps_fns)) {
    walk_recursive(expr, results, locals, restrict)
  }
}

analyze_arrow <- function(expr, results, locals, restrict) {
  walk_recursive(
    flatten_assignment(expr[[2]]),
    results,
    locals,
    restrict
  )
  ignore(walk_code)(expr[[3]], results, locals, restrict)
  ht_set(locals, get_assigned_var(expr))
}

analyze_knitr_in <- function(expr, results, restrict) {
  warn_nonliteral_paths(expr)
  expr <- ignore_ignore(expr)
  files <- analyze_strings(expr[-1])
  lapply(
    files,
    analyze_knitr_file,
    results = results,
    restrict = NULL
  )
  ht_set(results$knitr_in, reencode_path(files))
}

analyze_file_in <- function(expr, results) {
  warn_nonliteral_paths(expr)
  expr <- ignore_ignore(expr)
  x <- analyze_strings(expr[-1])
  x <- file.path(x)
  x <- reencode_path(x)
  ht_set(results$file_in, x)
}

analyze_file_out <- function(expr, results) {
  warn_nonliteral_paths(expr)
  expr <- ignore_ignore(expr)
  x <- analyze_strings(expr[-1])
  x <- file.path(x)
  x <- reencode_path(x)
  ht_set(results$file_out, x)
}

warn_nonliteral_paths <- function(expr) {
  syms <- all.vars(expr, functions = TRUE)
  syms <- unique(syms)
  ignore_these <- c("drake", "::", ":::", "c", "list")
  syms <- setdiff(syms, c(drake_symbols, ignore_these))
  if (!length(syms)) {
    return()
  }
  str <- safe_deparse(expr)
  warn0(
    "Detected ", str,
    ". File paths in file_in(), file_out(), and knitr_in() ",
    "must be literal strings, not variables. For example, ",
    "file_in(\"file1.csv\", \"file2.csv\") is legal, but ",
    "file_in(paste0(filename_variable, \".csv\")) is not. ",
    "Details: https://books.ropensci.org/drake/plans.html#static-files"
  )
}

analyze_knitr_file <- function(file, results, restrict) {
  if (!length(file)) {
    return(new_drake_deps())
  }
  fragments <- get_tangled_frags(file)
  out <- ignore(drake_deps_ht)(
    fragments,
    restrict = restrict
  )
  if (length(out)) {
    knitr_in_slots <- c(
      "knitr_in",
      "file_in",
      "file_out",
      "loadd",
      "readd"
    )
    for (slot in knitr_in_slots) {
      ht_merge(results[[slot]], out[[slot]])
    }
  }
}

analyze_namespaced <- function(expr, results, locals, restrict) {
  x <- safe_deparse(expr, backtick = TRUE)
  if (!ht_exists(locals, x)) {
    ht_set(results$namespaced, reencode_namespaced(x))
  }
}

analyze_loadd <- function(expr, results, restrict) {
  expr <- ignore_ignore(expr)
  expr <- match.call(drake::loadd, as.call(expr))
  expr <- expr[-1]
  ht_set(results$loadd, analyze_strings(expr["list"]))
  index <- which(is_unnamed(expr))
  if (!length(index)) {
    return()
  }
  dots <- expr[index]
  strings <- analyze_strings(dots)
  symbols <- safe_all_vars(dots)
  if (!is.null(restrict)) {
    strings <- ht_filter(restrict, strings)
    symbols <- ht_filter(restrict, symbols)
  }
  ht_set(results$loadd, strings)
  ht_set(results$loadd, symbols)
}

analyze_readd <- function(expr, results, restrict) {
  expr <- ignore_ignore(expr)
  expr <- match.call(drake::readd, as.call(expr))
  strings <- analyze_strings(expr["target"])
  symbols <- safe_all_vars(expr["target"])
  if (!is.null(restrict)) {
    strings <- ht_filter(restrict, strings)
    symbols <- ht_filter(restrict, symbols)
  }
  ht_set(results$readd, strings)
  ht_set(results$readd, symbols)
}

analyze_assign <- function(expr, results, locals, restrict) {
  expr <- match.call(definition = assign, call = expr)
  if (is.character(expr$x)) {
    ht_set(results$strings, expr$x)
    is_local <- is.null(expr$pos) || identical(expr$pos, formals(assign)$pos)
    if (is_local) {
      ht_set(locals, expr$x)
    }
  } else {
    ignore(walk_code)(expr$x, results, locals, restrict)
  }
  expr$x <- NULL
  walk_recursive(expr, results, locals, restrict)
}

analyze_delayed_assign <- function(expr, results, locals, restrict) {
  expr <- match.call(definition = delayedAssign, call = expr)
  if (is.character(expr$x)) {
    ht_set(results$strings, expr$x)
    is_local <- is.null(expr$assign.env) ||
      identical(expr$assign.env, formals(delayedAssign)$assign.env)
    if (is_local) {
      ht_set(locals, expr$x)
    }
  } else {
    analyze_global(expr$x, results, locals, restrict)
  }
  expr$x <- NULL
  walk_recursive(expr, results, locals, restrict)
}

analyze_function <- function(expr, results, locals, restrict) {
  expr <- unwrap_function(expr)
  if (typeof(expr) != "closure") {
    return()
  }
  locals <- ht_clone(locals)
  ht_set(locals, names(formals(expr)))
  ignore(walk_code)(formals(expr), results, locals, restrict)
  ignore(walk_code)(body(expr), results, locals, restrict)
}

analyze_usemethod <- function(expr, results, locals, restrict) {
  generic <- expr[["generic"]] %|||% expr[[2]]
  if (!is.character(generic) || length(generic) != 1L) {
    return()
  }
  pattern <- gsub(".", "\\.", generic, fixed = TRUE)
  pattern <- sprintf("^%s\\.", pattern)
  methods <- grep(
    pattern = pattern,
    x = ht_list(restrict),
    value = TRUE
  )
  lapply(
    X = methods,
    FUN = analyze_global,
    results = results,
    locals = locals,
    restrict = restrict
  )
  invisible()
}

analyze_strings <- function(expr) {
  ht <- ht_new()
  walk_strings(expr, ht)
  ht_list(ht)
}

analyze_for <- function(expr, results, locals, restrict) {
  ht_set(locals, as.character(expr[[2]]))
  walk_recursive(expr[-2], results, locals, restrict)
}

analyze_global <- function(expr, results, locals, restrict) {
  x <- as.character(expr)
  if (!nzchar(x)) {
    return()
  }
  if (ht_exists(locals, x)) {
    return()
  }
  if (is.null(restrict) || ht_exists(restrict, x)) {
    ht_set(results$globals, x)
  }
}

walk_recursive <- function(expr, results, locals, restrict) {
  lapply(
    X = expr,
    FUN = ignore(walk_code),
    results = results,
    locals = locals,
    restrict = restrict
  )
  invisible()
}

walk_strings <- function(expr, ht) {
  if (!length(expr)) {
    return()
  } else if (is.function(expr)) {
    walk_strings(formals(expr), ht)
    walk_strings(body(expr), ht)
  } else if (is.character(expr)) {
    expr <- Filter(x = expr, f = nzchar)
    ht_set(ht, expr)
  } else if (is.pairlist(expr) || is_callish(expr)) {
    lapply(expr, walk_strings, ht = ht)
  }
  invisible()
}

ignore_ignore <- function(x) {
  if (is.function(x) && !is.primitive(x) && !is.null(body(x))) {
    body(x) <- ignore_ignore(body(x))
  } else if (is_callish(x)) {
    if (safe_deparse(x[[1]], backtick = FALSE) %in% ignore_fns) {
      x <- quote(ignore())
    } else {
      x[] <- lapply(as.list(x), ignore_ignore)
    }
  }
  x
}

is_callish <- function(x) {
  length(x) > 0 && is.language(x) && (is.call(x) || is.recursive(x))
}

get_tangled_frags <- function(file) {
  if (!length(file)) {
    return(character(0))
  }
  if (!file.exists(file)) {
    warn0("Could not open ", file, " to detect dependencies.")
    return(character(0))
  }
  fragments <- tryCatch({
    parse(text = get_tangled_text(file))
  },
  error = function(e) {
    warn0(
      "Could not parse ",
      file,
      " to detect dependencies: ",
      conditionMessage(e)
    )
    character(0)
  })
}

# From https://github.com/duncantl/CodeDepends/blob/3e2e53f5794eea169117bd1b2f96801b813b22fd/R/sweave.R#L15 # nolint
get_tangled_text <- function(doc) {
  assert_pkg("knitr")
  id <- make.names(tempfile(), unique = FALSE, allow_ = TRUE)
  con <- textConnection(id, "w", local = TRUE)
  on.exit(close(con))
  with_options(
    new = list(knitr.purl.inline = TRUE),
    code = knitr::knit(doc, output = con, tangle = TRUE, quiet = TRUE)
  )
  textConnectionValue(con)
}

# Functions borrowed directly from codetools to deal with
# nested replacement functions:
# https://github.com/cran/codetools/blob/9bac1daaf19a36bd03a2cd7d67041893032e7a04/R/codetools.R#L302-L365 # nolint
# The code is following the subset assignment section
# of the R language definition manual:
# https://cran.r-project.org/doc/manuals/R-lang.html#Subset-assignment
flatten_assignment <- function(e) {
  if (typeof(e) != "language") {
    return()
  }
  if (e[[1]] == quote(`$`) || e[[1]] == quote(`@`)) {
    return(evalseq(e[[2]]))
  }
  c(evalseq(e[[2]]), apdef(e))
}

apdef <- function(e) {
  v <- NULL
  while (typeof(e) == "language") {
    ef <- e
    ef[[1]] <- make_assignment_fn(e[[1]])
    if (typeof(ef[[2]]) == "language") {
      ef[[2]] <- codetools_tmp
    }
    ef$value <- codetools_tmpv
    v <- c(v, list(ef))
    e <- e[[2]]
  }
  v
}

get_assigned_var <- function(e) {
  v <- e[[2]]
  if (missing(v)) {
    stop0(paste("missing assignment variable in", dsq(e)))
  } else if (typeof(v) %in% c("symbol", "character")) {
    as.character(v)
  } else {
    while (typeof(v) == "language") {
      if (length(v) < 2) {
        stop0(paste("unfinished code:", dsq(e)))
      }
      v <- v[[2]]
      if (missing(v)) {
        stop0(paste("missing variable in", dsq(e)))
      }
    }
    if (typeof(v) != "symbol") {
      stop0(paste("not a symbol:", dsq(e)))
    }
    as.character(v)
  }
}

make_assignment_fn <- function(fun) {
  if (typeof(fun) == "symbol") {
    return(make_assignment_fn_symbol(fun))
  } else {
    make_assignment_fn_impl(fun)
  }
}

make_assignment_fn_symbol <- function(fun) {
  as.name(paste0(as.character(fun), "<-"))
}

make_assignment_fn_impl <- function(fun) {
  is_correctly_namespaced <- typeof(fun) == "language" &&
    typeof(fun[[1]]) == "symbol" &&
    as.character(fun[[1]]) %in% c("::", ":::") &&
    length(fun) == 3 &&
    typeof(fun[[3]]) == "symbol"
  if (is_correctly_namespaced) {
    fun[[3]] <- as.name(paste0(as.character(fun[[3]]), "<-"))
    fun
  } else {
    stop0("bad function in complex assignments: ", dsq(fun))
  }
}

dsq <- function(e) {
  sQuote(safe_deparse(e, backtick = TRUE))
}

evalseq <- function(e) {
  if (typeof(e) == "language") {
    v <- evalseq(e[[2]])
    e[[2]] <- codetools_tmp
    c(v, list(e))
  } else {
    list(e)
  }
}

unwrap_function <- function(funct) {
  if (is_vectorized(funct)) {
    funct <- environment(funct)[["FUN"]]
  }
  funct
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

is_unnamed <- function(x) {
  if (!length(names(x))) {
    rep(TRUE, length(x))
  } else {
    !nzchar(names(x))
  }
}

safe_all_vars <- function(expr) {
  out <- lapply(expr, all.vars)
  as.character(unlist(out))
}

direct_deparse <- function(...) {
  make_direct_deparse()(...)
}

make_direct_deparse <- function() {
  .deparseOpts <- identity
  environment(deparse) <- environment()
  deparse
}

deparse_control_custom <- .deparseOpts(c("keepNA", "keepInteger"))
deparse_control_default <- .deparseOpts(eval(formals(deparse)$control))

safe_deparse_function <- function(x) {
  if (is.function(x)) {
    x <- safe_deparse(x, backtick = TRUE)
  }
  x
}

safe_deparse <- function(x, collapse = "\n", backtick = TRUE) {
  out <- direct_deparse(
    x,
    control = deparse_control_custom,
    backtick = backtick
  )
  if (length(out) > 1L) {
    out <- paste(out, collapse = collapse)
  }
  out
}

pair_text <- function(x, y) {
  apply(expand.grid(x, y), 1, paste0, collapse = "")
}

codetools_tmp_str <- "*ct_tmp*"
codetools_tmpv_str <- "*ct_tmpv*"
codetools_tmp <- as.name(codetools_tmp_str)
codetools_tmpv <- as.name(codetools_tmpv_str)
drake_prefix <- c("", "drake::", "drake:::")
file_in_fns <- pair_text(drake_prefix, c("file_in"))
file_out_fns <- pair_text(drake_prefix, c("file_out"))
ignore_fns <- pair_text(drake_prefix, "ignore")
knitr_in_fns <- pair_text(drake_prefix, c("knitr_in"))
loadd_fns <- pair_text(drake_prefix, "loadd")
readd_fns <- pair_text(drake_prefix, "readd")
target_fns <- pair_text(drake_prefix, "target")
trigger_fns <- pair_text(drake_prefix, "trigger")
drake_plan_fns <- pair_text(drake_prefix, "drake_plan")
no_deps_fns <- c(
  pair_text(drake_prefix, "no_deps"),
  ignore_fns,
  drake_plan_fns
)
file_fns <- c(file_in_fns, file_out_fns, knitr_in_fns)
drake_symbols <- sort(
  c(
    codetools_tmp_str,
    codetools_tmpv_str,
    file_in_fns,
    file_out_fns,
    knitr_in_fns,
    loadd_fns,
    no_deps_fns,
    readd_fns,
    target_fns,
    trigger_fns
  )
)

base_symbols <- sort(
  grep(
    pattern = "^[\\.a-zA-Z]",
    x = ls("package:base"),
    value = TRUE,
    invert = TRUE
  )
)

bad_symbols <- sort(
  c(
    ".",
    "..",
    ".gitignore",
    "Thumbs.db"
  )
)

ignored_symbols <- sort(c(drake_symbols, base_symbols, bad_symbols))
ignored_symbols_list <- as.list(rep(TRUE, length(ignored_symbols)))
names(ignored_symbols_list) <- ignored_symbols
