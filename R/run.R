run_command <- function(target, command, config, seed){
  retries <- 0
  while (retries <= config$retries){
    exception <- tryCatch({
        withr::with_seed(seed, {
          value <- with_timeout(
            target = target,
            command = command,
            config = config
          )
        })
        return(value)
      },
      error = function(exception){
        write(
          x = paste("Error:", exception$message),
          file = stderr()
        )
        return(exception)
      }
    )
    retries <- retries + 1
    if (config$verbose & retries <= config$retries){
      text <- paste0("retry ", target, ": ", retries, " of ", config$retries)
      finish_console(text = text, message = "retry")
    }
  }
  give_up(target = target, exception = exception, config = config)
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

give_up <- function(target, exception, config){
  config$cache$set(key = target, value = "failed",
    namespace = "progress")
  text <- paste("fail", target)
  if (config$verbose){
    finish_console(text = text, message = "fail")
  }
  stop(exception)
}
