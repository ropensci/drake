warn_now <- function(target, warnings){
  if (!length(warnings)){
    return()
  }
  warn_opt <- max(1, getOption("warn"))
  withr::with_options(
    new = list(warn = warn_opt),
    warning(
      "target ", target, ":\n",
      multiline_message(warnings),
      call. = FALSE
    )
  )
}

# We may just want to have a warning here.
handle_build_error <- function(target, meta, config){
  if (!inherits(meta$error, "error")){
    return()
  }
  if (config$verbose){
    text <- paste("fail", target)
    finish_console(text = text, pattern = "fail", verbose = config$verbose)
  }
  stop(
    "Target '", target, "' failed. Use diagnose(", target,
    ") to retrieve more diagnostic information.",
    call. = FALSE
  )
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
