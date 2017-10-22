run_command <- function(target, command, config, seed = seed){
  withr::with_seed(seed, {
    with_retries(target = target, command = command, config = config)
  })
}

with_retries <- function(target, command, config){
  retries <- 0
  while (retries <= config$retries){
    try({
      value <- with_timeout(
        target = target,
        command = command,
        config = config
      )
      return(value)
    },
    silent = FALSE)
    retries <- retries + 1
    if (config$verbose & retries <= config$retries){
      text <- paste0("retry ", target, ": ", retries, " of ", config$retries)
      finish_console(text = text, message = "retry")
    }
  }
  declare_failure(target = target, config = config)
}

with_timeout <- function(target, command, config){
  tryCatch({
    R.utils::evalWithTimeout({
      value <- eval(parse(text = command), envir = config$envir)
    },
    timeout = config$timeout,
    cpu = config$cpu,
    elapsed = config$elapsed,
    onTimeout = "error")
  },
  error = function(e){
    catch_timeout(e, target = target, config = config)
  })
}
  
catch_timeout <- function(e, target, config){
  if ("TimeoutException" %in% class(e)){
    limits <- gsub(pattern = "^[^\\[]*", "", e$message)
    text <- paste("timeout", target, limits)
    if (config$verbose){
      finish_console(text = text, message = "timeout")
    }
    stop(e$message)
  } else {
    stop(e$message)
  }
}

declare_failure <- function(target, config){
  config$cache$set(key = target, value = "failed",
    namespace = "progress")
  text <- paste("fail", target)
  if (config$verbose){
    finish_console(text = text, message = "fail")
  }
  stop(
    "Target ", target, " failed.\n",
    "No traceback available.\n",
    "To debug, use debug() on one of your functions or ",
    "call make() with browser() in the appropriate ",
    "workflow plan command."
  )
}
