run_command <- function(target, command, seed, config){
  retries <- 0
  max_retries <- workplan_override(
    target = target,
    field = "retries",
    config = config
  ) %>%
    as.numeric
  while (retries <= max_retries){
    value <- one_try(
      target = target,
      command = command,
      seed = seed,
      config = config
    )
    if (!inherits(value, "error")){
      return(value)
    }
    write(
      x = paste0("Error building target ", target, ": ", value$message),
      file = stderr()
    )
    config$cache$set(
      key = target,
      value = value,
      namespace = "errors"
    )
    retries <- retries + 1
    console_retry(target = target, retries = retries, config = config)
  }
  give_up(target = target, config = config)
}

one_try <- function(target, command, seed, config){
  withr::with_seed(seed, {
    with_timeout(
      target = target,
      command = command,
      config = config
    )
  })
}

with_timeout <- function(target, command, config){
  env <- environment()
  command <- wrap_in_try_statement(target = target, command = command)
  timeouts <- resolve_timeouts(target = target, config = config)
  R.utils::withTimeout({
      value <- eval(parse(text = command), envir = env)
    },
    timeout = timeouts["timeout"],
    cpu = timeouts["cpu"],
    elapsed = timeouts["elapsed"],
    onTimeout = "error"
  )
}

resolve_timeouts <- function(target, config){
  timeouts <- sapply(
    X = c("timeout", "cpu", "elapsed"),
    FUN = function(field){
      workplan_override(
        target = target,
        field = field,
        config = config
      ) %>%
        as.numeric
    },
    simplify = FALSE,
    USE.NAMES = TRUE
  )
  for (field in c("cpu", "elapsed")){
    if (!length(timeouts[[field]])){
      timeouts[[field]] <- timeouts$timeout
    }
  }
  timeouts
}

give_up <- function(target, config){
  config$cache$set(key = target, value = "failed",
    namespace = "progress")
  text <- paste("fail", target)
  if (config$verbose){
    finish_console(text = text, pattern = "fail", verbose = config$verbose)
  }
  stop(
    "Target '", target, "' failed to build. ",
    "Use diagnose(", target,
    ") to retrieve diagnostic information.",
    call. = FALSE
  )
}

wrap_in_try_statement <- function(target, command){
  paste(
    target,
    "<- evaluate::try_capture_stack(quote({\n",
    command,
    "\n}), env = config$envir)"
  )
}
