handle_build_exceptions <- function(target, meta, config) {
  if (length(meta$warnings) && config$verbose) {
    warn_opt <- max(1, getOption("warn"))
    with_options(
      new = list(warn = warn_opt),
      warning(
        "target ", target, " warnings:\n",
        multiline_message(meta$warnings),
        call. = FALSE
      )
    )
  }
  if (length(meta$messages) && config$verbose) {
    message(
      "Target ", target, " messages:\n",
      multiline_message(meta$messages)
    )
  }
  if (inherits(meta$error, "error")) {
    if (config$verbose) {
      text <- paste("fail", target)
      finish_console(text = text, pattern = "fail", config = config)
    }
    store_failure(target = target, meta = meta, config = config)
    if (!config$keep_going) {
      drake_error(
        "Target `", target, "` failed. Call `diagnose(", target,
        ")` for details. Error message:\n  ",
        meta$error$message,
        config = config
      )
    }
  }
}

error_character0 <- function(e) {
  character(0)
}

error_false <- function(e) {
  FALSE
}

error_na <- function(e) {
  NA_character_
}

error_null <- function(e) {
  NULL
}

error_tibble_times <- function(e) {
  stop(
    "Failed converting a data frame of times to a tibble. ",
    "Please install version 1.2.1 or greater of the pillar package.",
    call. = FALSE
  )
}

# Should be used as sparingly as possible.
just_try <- function(code) {
  try(suppressWarnings(code), silent = TRUE)
}

mention_pure_functions <- function(e) {
  msg1 <- "locked binding"
  msg2 <- "locked environment"
  locked_envir <- grepl(msg1, e$message) || grepl(msg2, e$message)
  if (locked_envir) {
    e$message <- paste0(e$message, ". ", locked_envir_msg)
  }
  e
}

locked_envir_msg <- paste(
  "\nPlease read the \"Self-invalidation\"",
  "section of the make() help file."
)

prepend_fork_advice <- function(msg) {
  if (!length(msg)) {
    return(msg)
  }
  # Loop so we can use fixed = TRUE, which is fast. # nolint
  fork_error <- sum(vapply(
    c("parallel", "core"),
    function(pattern) any(grepl(pattern, msg, fixed = TRUE)),
    FUN.VALUE = logical(1)
  ))
  if (!fork_error) {
    return(msg)
  }
  out <- paste(
    "\n Having problems with parallel::mclapply(),",
    "future::future(), or furrr::future_map() in drake?",
    "Try one of the workarounds at",
    "https://ropenscilabs.github.io/drake-manual/hpc.html#parallel-computing-within-targets", # nolint
    "or https://github.com/ropensci/drake/issues/675. \n\n"
  )
  c(out, msg)
}
