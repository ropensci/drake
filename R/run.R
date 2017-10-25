run_command <- function(target, command, seed, config){
  retries <- 0
  while (retries <= config$retries){
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
    if (config$verbose & retries <= config$retries){
      text <- paste0("retry ", target, ": ", retries, " of ", config$retries)
      finish_console(text = text, message = "retry")
    }
  }
  give_up(target = target, config = config)
}

one_try <- function(target, command, seed, config){
  evaluate::try_capture_stack({
    withr::with_seed(seed, {
      with_timeout(
        target = target,
        command = command,
        config = config
      )
    })
  }, env = parent.frame())
}

with_timeout <- function(target, command, config){
  tryCatch({
    R.utils::withTimeout({
      value <- eval(parse(text = command), envir = config$envir)
    },
    timeout = config$timeout,
    cpu = config$cpu,
    elapsed = config$elapsed,
    onTimeout = "error")
  },
  TimeoutException = function(e){
    catch_timeout(target = target, config = config)
  })
}
  
catch_timeout <- function(target, config){
  text <- paste0("timeout ", target, ": cpu = ",
    config$cpu, "s, elapsed = ", config$elapsed, "s")
  if (config$verbose){
    finish_console(text = text, message = "timeout")
  }
  stop(text, call. = FALSE)
}

give_up <- function(target, config){
  config$cache$set(key = target, value = "failed",
    namespace = "progress")
  text <- paste("fail", target)
  if (config$verbose){
    finish_console(text = text, message = "fail")
  }
  stop(
    "Target ", target, " failed. ",
    "Use diagnose(", target,
    ") to retrieve diagnostic information."
  )
}
