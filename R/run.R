one_build <- function(target, meta, config){
  timeouts <- resolve_timeouts(target = target, config = config)
  R.utils::withTimeout(
    withr::with_seed(
      meta$seed,
      run_command(
        target = target,
        meta = meta,
        config = config
      )
    ),
    timeout = timeouts["timeout"],
    cpu = timeouts["cpu"],
    elapsed = timeouts["elapsed"],
    onTimeout = "error"
  )
}

# Borrowed from the rmonad package
# https://github.com/arendsee/rmonad/blob/14bf2ef95c81be5307e295e8458ef8fb2b074dee/R/to-monad.R#L68 # nolint
run_command <- function(target, meta, config){
  warnings <- messages <- NULL
  parsed_command <- preprocess_command(target = target, config = config)
  meta$output <- capture.output(
    tmp <- capture.output(
      meta$time_command <- system.time(
        withCallingHandlers(
          value <- evaluate::try_capture_stack(
            quoted_code = parsed_command,
            env = config$envir
          ),
          warning = function(w){
            warnings <<- c(warnings, w$message)
          },
          message = function(m){
            msg <- gsub(pattern = "\n$", replacement = "", x = m$message)
            messages <<- c(messages, msg)
          }
        ),
        gcFirst = FALSE # for performance
      ),
      type = "message"
    ),
    type = "output"
  )
  meta$warnings <- warnings
  meta$messages <- messages
  if (inherits(value, "error")){
    meta$error <- value
    value <- NULL
  }
  list(
    target = target,
    meta = meta,
    value = value
  )
}

resolve_timeouts <- function(target, config){
  keys <- c("timeout", "cpu", "elapsed")
  timeouts <- lapply(
    X = keys,
    FUN = function(field){
      drake_plan_override(
        target = target,
        field = field,
        config = config
      ) %>%
        as.numeric
    }
  )
  names(timeouts) <- keys
  for (field in c("cpu", "elapsed")){
    if (!length(timeouts[[field]])){
      timeouts[[field]] <- timeouts$timeout
    }
  }
  timeouts
}
