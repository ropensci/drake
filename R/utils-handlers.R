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
  "Your `drake` code tried to modify some pre-existing data",
  "in your environment.",
  "This sort of thing undermines reproducibility",
  "(example: https://github.com/ropensci/drake/issues/664#issuecomment-453163562)", # nolint
  "so `drake` stops you by default.",
  "All your commands and functions should return *new* output",
  "and never modify existing targets or potential imports.",
  "Beware <<-, ->>, attach(), and data().",
  "Also please try to avoid options() even though drake does not stop you.",
  "Use make(lock_envir = FALSE) to avoid this error",
  "(not recommended)."
)
