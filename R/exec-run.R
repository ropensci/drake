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

# The beginnings of with_handling() were borrowed from the rmonad package.
# Lots of modifications since.
# Copyright Zebulun Arendsee, GPL-3:
# https://github.com/arendsee/rmonad/blob/14bf2ef95c81be5307e295e8458ef8fb2b074dee/R/to-monad.R#L68 # nolint
with_handling <- function(target, meta, config) {
  warnings <- messages <- NULL
  start <- proc.time()
  withCallingHandlers(
    value <- with_call_stack(target = target, config = config),
    warning = function(w) {
      drake_log_warning(w$message, config = config)
      warnings <<- c(warnings, w$message)
      invokeRestart("muffleWarning")
    },
    message = function(m) {
      msg <- gsub(pattern = "\n$", replacement = "", x = m$message)
      drake_log_message(msg, config = config)
      messages <<- c(messages, msg)
      invokeRestart("muffleMessage")
    }
  )
  meta$time_command <- proc.time() - start
  meta$warnings <- warnings
  meta$messages <- messages
  if (inherits(value, "error")) {
    meta$error <- value
    value <- NULL
  }
  list(
    target = target,
    meta = meta,
    value = value
  )
}

# Taken directly from the `evaluate::try_capture_stack()`.
# https://github.com/r-lib/evaluate/blob/b43d54f1ea2fe4296f53316754a28246903cd703/R/traceback.r#L20-L47 # nolint
# Copyright Hadley Wickham and Yihui Xie, 2008 - 2018. MIT license.
with_call_stack <- function(target, config) {
  frame <- sys.nframe()
  capture_calls <- function(e) {
    e <- mention_pure_functions(e)
    e$calls <- head(sys.calls()[-seq_len(frame + 7)], -2)
    signalCondition(e)
  }
  expr <- config$layout[[target]]$command_build
  # Need to make sure the environment is locked for running commands.
  # Why not just do this once at the beginning of `make()`?
  # Because do_prework() and future::value()
  # may need to modify the global state.
  # Unfortunately, we have to repeatedly lock and unlock the envir.
  # Unfortunately, the safe way to do this adds overhead and
  # makes future::multicore parallelism serial.
  if (config$lock_envir) {
    i <- 1
    # Lock the environment only while running the command.
    while (environmentIsLocked(config$envir)) {
      Sys.sleep(config$sleep(max(0L, i))) # nocov
      i <- i + 1 # nocov
    }
    lock_environment(config$envir)
    on.exit(unlock_environment(config$envir))
  }
  config$eval[[drake_target_marker]] <- target
  tidy_expr <- eval(expr = expr, envir = config$eval) # tidy eval prep
  tryCatch(
    withCallingHandlers(
      eval(expr = tidy_expr, envir <- config$eval), # pure eval
      error = capture_calls
    ),
    error = identity
  )
}

# Taken from `R.utils::withTimeout()` and simplified.
# https://github.com/HenrikBengtsson/R.utils/blob/13e9d000ac9900bfbbdf24096d635da723da76c8/R/withTimeout.R # nolint
# Copyright Henrik Bengtsson, LGPL >= 2.1.
with_timeout <- function(expr, cpu, elapsed) {
  expr <- substitute(expr)
  envir <- parent.frame()
  setTimeLimit(cpu = cpu, elapsed = elapsed, transient = TRUE)
  on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE))
  eval(expr, envir = envir)
}

resolve_timeouts <- function(target, config) {
  layout <- config$layout[[target]] %||% list()
  vapply(
    X = c("cpu", "elapsed"),
    FUN = function(key) {
      layout[[key]] %||NA% config[[key]]
    },
    FUN.VALUE = numeric(1)
  )
}
