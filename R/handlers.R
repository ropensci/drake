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
    stop(
      "Target '", target, "' failed. Use diagnose(", target,
      ") for details.",
      call. = FALSE
    )
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
