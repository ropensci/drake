handle_build_exceptions <- function(target, meta, config){
  if (length(meta$warnings) && config$verbose){
    warn_opt <- max(1, getOption("warn"))
    withr::with_options(
      new = list(warn = warn_opt),
      warning(
        "target ", target, " warnings:\n",
        multiline_message(meta$warnings),
        call. = FALSE
      )
    )
  }
  if (length(meta$messages) && config$verbose){
    message(
      "Target ", target, " messages:\n",
      multiline_message(meta$messages)
    )
  }
  if (inherits(meta$error, "error")){
    if (config$verbose){
      text <- paste("fail", target)
      finish_console(text = text, pattern = "fail", verbose = config$verbose)
    }
    store_failure(target = target, meta = meta, config = config)
    if (!config$keep_going){
      stop(
        "Target `", target, "`` failed. Call `diagnose(", target,
        ")` for details. Error message:\n  ",
        meta$error$message,
        call. = FALSE
      )
    }
  }
}

error_character0 <- function(e){
  character(0)
}

error_false <- function(e){
  FALSE
}

error_na <- function(e){
  NA
}

error_null <- function(e){
  NULL
}

error_tibble_times <- function(e){
  stop(
    "Failed converting a data frame of times to a tibble. ",
    "Please install version 1.2.1 or greater of the pillar package.",
    call. = FALSE
  )
}
