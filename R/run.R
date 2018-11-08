with_seed_timeout <- function(target, meta, config) {
  timeouts <- resolve_timeouts(target = target, config = config)
  with_timeout(
    withr::with_seed(
      meta$seed,
      with_handling(
        target = target,
        meta = meta,
        config = config
      )
    ),
    cpu = timeouts$cpu,
    elapsed = timeouts$elapsed
  )
}

# Borrowed from the rmonad package
# https://github.com/arendsee/rmonad/blob/14bf2ef95c81be5307e295e8458ef8fb2b074dee/R/to-monad.R#L68 # nolint
with_handling <- function(target, meta, config) {
  warnings <- messages <- NULL
  capture.output(
    meta$time_command <- system.time(
      withCallingHandlers(
        value <- with_call_stack(target = target, config = config),
        warning = function(w) {
          warnings <<- c(warnings, w$message)
        },
        message = function(m) {
          msg <- gsub(pattern = "\n$", replacement = "", x = m$message)
          messages <<- c(messages, msg)
        }
      ),
      gcFirst = FALSE # for performance
    ),
    type = "message"
  )
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
with_call_stack <- function (target, config) {
  capture_calls <- function(e) {
    e["call"] <- e["call"]
    e$calls <- head(sys.calls()[-seq_len(frame + 7)], -2)
    signalCondition(e)
  }
  expr <- config$ordinances[[target]]$command_build
  frame <- sys.nframe()
  tryCatch(
    withCallingHandlers(
      eval(expr = expr, envir <- config$envir),
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
  keys <- c("timeout", "cpu", "elapsed")
  timeouts <- lapply(
    X = keys,
    FUN = function(field) {
      out <- drake_plan_override(
        target = target,
        field = field,
        config = config
      )
      as.numeric(out)
    }
  )
  names(timeouts) <- keys
  for (field in c("cpu", "elapsed")) {
    if (!length(timeouts[[field]])) {
      timeouts[[field]] <- timeouts$timeout
    }
  }
  timeouts
}
