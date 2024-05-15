local_build <- function(target, config, downstream) {
  meta <- drake_meta_(target, config)
  if (handle_triggers(target, meta, config)) {
    return()
  }
  announce_build(target, config)
  manage_memory(
    target,
    config,
    downstream = downstream,
    jobs = config$settings$jobs_preprocess
  )
  build <- try_build(target, meta, config)
  conclude_build(build, config)
}

announce_build <- function(target, config) {
  if (is_dynamic(target, config)) {
    announce_dynamic(target, config)
    return()
  }
  announce_static(target, config)
}

announce_static <- function(target, config) {
  set_progress(
    target = target,
    value = "running",
    config = config
  )
  action <- ifelse(is_subtarget(target, config), "subtarget", "target")
  config$logger$target(target, action)
}

announce_dynamic <- function(target, config) {
  set_progress(
    target = target,
    value = "running",
    config = config
  )
  action <- ifelse(
    is_registered_dynamic(target, config),
    "finalize",
    "dynamic"
  )
  config$logger$target(target, action)
}

try_build <- function(target, meta, config) {
  if (config$settings$garbage_collection) {
    on.exit(gc())
  }
  if (is_dynamic(target, config)) {
    return(dynamic_build(target, meta, config))
  }
  retries <- 0L
  spec <- config$spec[[target]] %|||% list()
  max_retries <- spec$retries
  if (is.null(max_retries) || is.na(max_retries)) {
    max_retries <- config$retries
  }
  max_retries <- as.integer(max_retries)
  while (retries <= max_retries) {
    if (retries > 0L) {
      config$logger$target(target, "retry")
    }
    build <- with_seed_timeout(
      target = target,
      meta = meta,
      config = config
    )
    if (!inherits(build$meta$error, "error")) {
      return(build)
    }
    retries <- retries + 1L
  }
  build
}

with_seed_timeout <- function(target, meta, config) {
  timeouts <- resolve_timeouts(target = target, config = config)
  with_timeout(
    with_seed(
      meta$seed,
      with_handling(
        target = target,
        meta = meta,
        config = config
      )
    ),
    cpu = timeouts[["cpu"]],
    elapsed = timeouts[["elapsed"]]
  )
}

resolve_timeouts <- function(target, config) {
  spec <- config$spec[[target]] %|||% list()
  vapply(
    X = c("cpu", "elapsed"),
    FUN = function(key) {
      out <- spec[[key]]
      if (is.null(out) || is.na(out)) {
        out <- config[[key]]
      }
      out
    },
    FUN.VALUE = numeric(1)
  )
}

# Taken from `R.utils::withTimeout()` and simplified.
# https://github.com/HenrikBengtsson/R.utils/blob/13e9d000ac9900bfbbdf24096d635da723da76c8/R/withTimeout.R # nolint
# Copyright Henrik Bengtsson, LGPL >= 2.1.
with_timeout <- function(expr, cpu, elapsed) {
  if (cpu < Inf || elapsed < Inf) {
    setTimeLimit(cpu = cpu, elapsed = elapsed, transient = TRUE)
    on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
  }
  expr <- substitute(expr)
  envir <- parent.frame()
  eval(expr, envir = envir)
}

# From withr https://github.com/r-lib/withr, copyright RStudio, GPL (>=2)
with_seed <- function(seed, code) {
  force(seed)
  with_preserve_seed({
    set.seed(seed)
    code
  })
}

# From withr https://github.com/r-lib/withr, copyright RStudio, GPL (>=2)
with_preserve_seed <- function(code) {
  old_seed <- get_valid_seed()
  on.exit(assign(".Random.seed", old_seed, globalenv()), add = TRUE)
  code
}

# From withr https://github.com/r-lib/withr, copyright RStudio, GPL (>=2)
get_valid_seed <- function() {
  seed <- get_seed()
  if (is.null(seed)) {
    # Trigger initialisation of RNG
    sample.int(1L) # nocov
    seed <- get_seed() # nocov
  }
  seed
}

# From withr https://github.com/r-lib/withr, copyright RStudio, GPL (>=2)
get_seed <- function() {
  no_seed_yet <- !exists(
    ".Random.seed",
    globalenv(),
    mode = "integer",
    inherits = FALSE
  )
  if (no_seed_yet) {
    return(NULL) # nocov
  }
  get(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
}

# The beginnings of with_handling() were borrowed from the rmonad package.
# Lots of modifications since.
# Copyright Zebulun Arendsee, GPL-3:
# https://github.com/arendsee/rmonad/blob/14bf2ef95c81be5307e295e8458ef8fb2b074dee/R/to-monad.R#L68 # nolint
with_handling <- function(target, meta, config) {
  warnings <- messages <- NULL
  if (config$settings$log_build_times) {
    start <- proc_time()
  }
  withCallingHandlers(
    value <- drake_with_call_stack_8a6af5(target = target, config = config),
    warning = function(w) {
      config$logger$disk(paste("Warning:", w$message), target = target)
      warnings <<- c(warnings, w$message)
      warning(as_immediate_condition(w))
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      msg <- gsub(pattern = "\n$", replacement = "", x = m$message)
      config$logger$disk(msg, target = target)
      messages <<- c(messages, msg)
      message(as_immediate_condition(m))
      invokeRestart("muffleMessage")
    }
  )
  if (config$settings$log_build_times) {
    meta$time_command <- proc_time() - start
  }
  meta$warnings <- warnings
  meta$messages <- messages
  if (inherits(value, "error")) {
    meta$error <- value
  }
  if (failed_fork(warnings)) {
    throw_fork_warning(config)
    meta <- prepend_fork_advice(meta)
  }
  list(target = target, meta = meta, value = value)
}

as_immediate_condition <- function(x) {
  class(x) <- c(class(x), "immediateCondition")
  x
}

throw_fork_warning <- function(config) {
  if (!delayed_relay(config)) {
    warn0(fork_advice())
  }
}

prepend_fork_advice <- function(meta) {
  meta$warnings <- c(meta$warnings, fork_advice())
  if (inherits(meta$error, "error")) {
    meta$error$message <- c(meta$error$message, fork_advice())
  }
  meta
}

failed_fork <- function(msg) {
  if (!length(msg)) {
    return(FALSE)
  }
  # Loop so we can use fixed = TRUE, which is fast. # nolint
  sum(vapply(
    c("parallel", "core"),
    function(pattern) any(grepl(pattern, msg, fixed = TRUE)),
    FUN.VALUE = logical(1)
  ))
}

fork_advice <- function(msg) {
  paste(
    "\n Having problems with parallel::mclapply(),",
    "future::future(), or furrr::future_map() in drake?",
    "Try one of the workarounds at",
    "https://books.ropensci.org/drake/hpc.html#parallel-computing-within-targets", # nolint
    "or https://github.com/ropensci/drake/issues/675. \n\n"
  )
}

# Taken directly from the `evaluate::try_capture_stack()`.
# https://github.com/r-lib/evaluate/blob/b43d54f1ea2fe4296f53316754a28246903cd703/R/traceback.r#L20-L47 # nolint
# Copyright Hadley Wickham and Yihui Xie, 2008 - 2018. MIT license.
drake_with_call_stack_8a6af5 <- function(target, config) {
  frame <- sys.nframe()
  capture_calls <- function(e) {
    calls <- vcapply(sys.calls(), safe_deparse)
    top_index <- min(which(grepl("^eval\\(expr = tidy_expr_8a6af5", calls)))
    top <- sys.frame(top_index + 7)
    bottom <- sys.frame(sys.nframe() - 2)
    e$calls <- deparse_traceback(rlang::trace_back(top = top, bottom = bottom))
    e <- mention_pure_functions(e)
    signalCondition(e)
  }
  expr <- config$spec[[target]]$command_build
  if (config$settings$lock_envir && !isNamespace(config$envir)) {
    message("Environment locking is not supported in drake >= 7.13.10")
  }
  tidy_expr_8a6af5 <- eval(expr = expr, envir = config$envir_subtargets)
  tryCatch(
    withCallingHandlers(
      eval(expr = tidy_expr_8a6af5, envir = config$envir_subtargets),
      error = capture_calls
    ),
    error = downsize_error,
    drake_cancel = cancellation
  )
}

downsize_error <- function(error) {
  structure(
    list(
      message = as.character(error$message),
      call = safe_deparse(error$call),
      calls = as.character(error$calls)
    ),
    class = class(error)
  )
}

# Prevents tracebacks from storing tons of data.
deparse_traceback <- function(traceback) {
  vcapply(traceback$call %|||% traceback$calls, safe_deparse)
}

unhidden_names <- function(envir) {
  out <- names(envir)
  out <- out[substr(out, 0, 1) != "."]
  out
}

mention_pure_functions <- function(e) {
  msg1 <- "locked binding"
  msg2 <- "locked environment"
  locked_envir <- grepl(msg1, e$message) || grepl(msg2, e$message)
  if (locked_envir) {
    e$message <- paste0(e$message, ". ", locked_envir_msg)
  }
  e
}

locked_envir_msg <- paste(
  "\nPlease read the \"Self-invalidation\"",
  "section of the make() help file."
)

conclude_build <- function(build, config) {
  target <- build$target
  value <- build$value
  meta <- build$meta
  conclude_build_impl(value, target, meta, config)
}

conclude_build_impl <- function(value, target, meta, config) {
  UseMethod("conclude_build_impl")
}

#' @export
conclude_build_impl.drake_cancel <- function(value, target, meta, config) { # nolint
  config$cache$set_progress(target = target, value = "cancelled")
  config$logger$target(target, "cancel")
}

#' @export
conclude_build_impl.default <- function(value, target, meta, config) {
  assert_output_files(target = target, meta = meta, config = config)
  handle_build_exceptions(target = target, meta = meta, config = config)
  value <- assign_format(target = target, value = value, config = config)
  store_outputs(target = target, value = value, meta = meta, config = config)
  assign_to_envir(target = target, value = value, config = config)
}

assign_format <- function(target, value, config) {
  format <- config$spec[[target]]$format %||NA% config$format
  drop_format <- is.null(format) ||
    is.na(format) ||
    is.null(value) ||
    (is_dynamic(target, config) && !is_subtarget(target, config)) ||
    inherits(value, "error")
  if (drop_format) {
    return(value)
  }
  config$logger$disk("format", format, target = target)
  out <- list(value = value)
  class(out) <- c(paste0("drake_format_", format), "drake_format")
  sanitize_format(x = out, target = target, config = config)
}

sanitize_format <- function(x, target, config) {
  UseMethod("sanitize_format")
}

#' @export
sanitize_format.default <- function(x, target, config) {
  x
}

#' @export
sanitize_format.drake_format_fst <- function(x, target, config) { # nolint
  if (!identical(class(x$value), "data.frame")) {
    msg <- paste0(
      "You selected fst format for target ", target,
      ", so drake will convert it from class ",
      safe_deparse(class(x$value), backtick = TRUE),
      " to a plain data frame."
    )
    warn0(msg)
    config$logger$disk(msg, target = target)
  }
  x$value <- as.data.frame(x$value)
  x
}

#' @export
sanitize_format.drake_format_fst_tbl <- function(x, target, config) { # nolint
  assert_pkg("tibble")
  if (!inherits(x$value, "tbl_df")) {
    msg <- paste0(
      "You selected fst_tbl format for target ", target,
      ", so drake will convert it from class ",
      safe_deparse(class(x$value), backtick = TRUE),
      " to a tibble."
    )
    warn0(msg)
    config$logger$disk(msg, target = target)
  }
  x$value <- tibble::as_tibble(x$value)
  x
}

#' @export
sanitize_format.drake_format_fst_dt <- function(x, target, config) { # nolint
  assert_pkg("data.table")
  if (!inherits(x$value, "data.table")) {
    msg <- paste0(
      "You selected fst_dt format for target ", target,
      ", so drake will convert it from class ",
      safe_deparse(class(x$value), backtick = TRUE),
      " to a data.table object."
    )
    warn0(msg)
    config$logger$disk(msg, target = target)
  }
  x$value <- data.table::as.data.table(x$value)
  x
}

#' @export
sanitize_format.drake_format_diskframe <- function(x, target, config) { # nolint
  assert_pkg("disk.frame")
  if (!inherits(x$value, "disk.frame")) {
    msg <- paste0(
      "You selected disk.frame format for target ", target,
      ", so drake will try to convert it from class ",
      safe_deparse(class(x$value), backtick = TRUE),
      " to a disk.frame object. For optimal performance ",
      "please create disk.frame objects yourself using an outdir ",
      "on the same drive as drake's cache ",
      "(say, with as.disk.frame(outdir = drake_tempfile()))."
    )
    warn0(msg)
    config$logger$disk(msg, target = target)
    x$value <- disk.frame::as.disk.frame(
      df = x$value,
      outdir = config$cache$file_tmp()
    )
  }
  x
}

#' @export
sanitize_format.drake_format_file <- function(x, target, config) { # nolint
  if (!is.character(x$value)) {
    msg <- paste0(
      "You selected the \"file\" format for target ", target,
      ", so the return value must be a character vector. ",
      "coercing to character."
    )
    config$logger$disk("Error:", msg, target = target)
    warn0(msg)
    x$value <- as.character(x$value)
  }
  x
}

assign_to_envir <- function(target, value, config) {
  if (is_subtarget(target, config)) {
    return()
  }
  memory_strategy <- config$spec[[target]]$memory_strategy
  if (is.null(memory_strategy) || is.na(memory_strategy)) {
    memory_strategy <- config$settings$memory_strategy
  }
  skip_memory <- memory_strategy %in% c("autoclean", "unload", "none")
  if (skip_memory) {
    return()
  }
  do_assign <- identical(config$settings$lazy_load, "eager") &&
    !is_encoded_path(target) &&
    !is_imported(target, config)
  if (do_assign) {
    assign(
      x = target,
      value = value_format(value, target, config),
      envir = config$envir_targets
    )
    config$envir_loaded$targets <- c(config$envir_loaded$targets, target)
  }
  invisible()
}

value_format <- function(value, target, config) {
  UseMethod("value_format")
}

#' @export
value_format.drake_format_diskframe <- function(value, target, config) { # nolint
  config$cache$get(target)
}

#' @export
value_format.drake_format <- function(value, target, config) {
  value$value
}

#' @export
value_format.default <- function(value, target, config) {
  value
}

assert_output_files <- function(target, meta, config) {
  deps <- config$spec[[target]]$deps_build
  if (!length(deps$file_out)) {
    return()
  }
  files <- unique(as.character(deps$file_out))
  files <- config$cache$decode_path(files)
  missing_files <- files[!file.exists(files)]
  if (length(missing_files)) {
    msg <- paste0(
      "Missing files for target ",
      target, ":\n",
      multiline_message(missing_files)
    )
    config$logger$disk(paste("Warning:", msg))
    warn0(msg)
  }
}

handle_build_exceptions <- function(target, meta, config) {
  relay <- delayed_relay(config)
  if (length(meta$warnings) && config$logger$verbose && relay) {
    handle_build_warnings(target, meta, config)
  }
  if (length(meta$messages) && config$logger$verbose && relay) {
    handle_build_messages(target, meta, config)
  }
  if (inherits(meta$error, "error")) {
    handle_build_error(target, meta, config)
  }
}

handle_build_warnings <- function(target, meta, config) {
  warn_opt <- max(1, getOption("warn"))
  with_options(
    new = list(warn = warn_opt),
    warn0(
      "target ", target, " warnings:\n",
      multiline_message(meta$warnings)
    )
  )
}

handle_build_messages <- function(target, meta, config) {
  cli_msg(
    "target", target, "messages:\n",
    multiline_message(meta$messages, indent = " ")
  )
}

handle_build_error <- function(target, meta, config) {
  config$logger$target(target, "fail")
  if (is_subtarget(target, config)) {
    parent <- config$spec[[target]]$subtarget_parent
    meta$subtarget <- target
    meta$subtarget_index <- which(target == config$spec[[parent]]$subtargets)
    store_failure(target = parent, meta = meta, config = config)
  }
  store_failure(target = target, meta = meta, config = config)
  if (!config$settings$keep_going) {
    log_failure(target, meta, config)
  }
}

log_failure <- function(target, meta, config) {
  msg1 <- paste0("target ", target, " failed.")
  diag <- paste0("diagnose(", target, ")$error")
  message <- paste(meta$error$message, collapse = "\n")
  trace <- paste(paste0("  ", meta$error$calls), collapse = "\n")
  msg2 <- paste0(diag, "$message:\n  ", message)
  msg3 <- paste0(diag, "$calls:\n", trace)
  msg <- paste(c(msg1, msg2, msg3), collapse = "\n")
  config$logger$disk(msg)
  stop0(msg)
}

delayed_relay <- function(config) {
  config$settings$parallelism == "clustermq"
}

# From withr https://github.com/r-lib/withr, copyright RStudio, GPL (>=2)
with_options <- function(new, code) {
  old <- set_options(new_options = new)
  on.exit(set_options(new_options = old))
  force(code)
}

# From withr https://github.com/r-lib/withr, copyright RStudio, GPL (>=2)
set_options <- function(new_options) {
  do.call(options, as.list(new_options))
}

store_failure <- function(target, meta, config) {
  set_progress(
    target = target,
    value = "failed",
    config = config
  )
  fields <- c(
    "messages",
    "warnings",
    "error",
    "seed",
    "subtarget",
    "subtarget_index"
  )
  fields <- intersect(fields, names(meta))
  meta <- meta[fields]
  config$cache$set(
    key = target,
    value = meta,
    namespace = "meta",
    use_cache = FALSE
  )
}

set_progress <- function(target, value, config) {
  skip_progress <- !identical(config$running_make, TRUE) ||
    !config$settings$log_progress ||
    (config$spec[[target]]$imported %||% FALSE)
  if (skip_progress) {
    return()
  }
  config$cache$set_progress(target = target, value = value)
}
