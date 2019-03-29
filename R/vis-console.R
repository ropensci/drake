console_msg <- function(
  ...,
  config,
  newline = config$verbose > 1L,
  color = colors["aux"]
) {
  if (is.null(config$verbose) || as.integer(config$verbose) < 1L) {
    return(invisible())
  }
  msg <- crop_text(paste(...))
  drake_log(msg, config = config)
  if (!is.null(color) && requireNamespace("crayon", quietly = TRUE)) {
    msg <- crayon::make_style(color)(msg)
  }
  utils::flush.console()
  tmp <- paste0(c("\r", rep(" ", getOption("width")), "\r"), collapse = "")
  message(tmp, appendLF = FALSE)
  message(msg, appendLF = newline)
}

drake_log <- function(msg, config) {
  if (is.null(config$console_log_file)) {
    return()
  }
  write(
    x = msg,
    file = config$console_log_file,
    append = TRUE
  )
  invisible()
}

console_time <- function(target, meta, config) {
  if (requireNamespace("lubridate", quietly = TRUE)) {
    exec <- round(lubridate::dseconds(meta$time_command$elapsed), 3)
    total <- round(lubridate::dseconds( meta$time_build$elapsed), 3)
    tail <- paste("", exec, "|", total, " (exec | total)")
  } else {
    tail <- " (install lubridate)" # nocov
  }
  console_msg("time  ", target, tail = tail, config = config)
}

drake_message <- function(..., config) {
  drake_log(paste(...), config = config)
  message(...)
}

drake_warning <- function(..., config) {
  drake_log(paste("Warning:", ...), config = config)
  warning(..., call. = FALSE)
}

drake_error <- function(..., config) {
  drake_log(paste("Error:", ...), config = config)
  stop(..., call. = FALSE)
}

crop_text <- Vectorize(function(x, width = getOption("width")) {
  if (nchar(x) > width) {
    x <- paste0(substr(x, 1, width - 3), "...")
  }
  x
},
"x", USE.NAMES = FALSE)

crop_lines <- function(x, n = 10) {
  if (length(x) > n) {
    x <- x[1:(n - 1)]
    x[n] <- "..."
  }
  x
}

multiline_message <- function(x) {
  n <- 30
  if (length(x) > n) {
    x <- c(x[1:(n - 1)], "...")
  }
  x <- paste0("  ", x)
  paste(x, collapse = "\n")
}

#' @title Show how a target/import was produced.
#' @description Show the command that produced a target
#'   or indicate that the object or file was imported.
#' @export
#' @param target Symbol denoting the target or import
#'   or a character vector if character_only is `TRUE`.
#' @param config A [drake_config()] list.
#' @param character_only Logical, whether to interpret
#'   `target` as a symbol (`FALSE`) or character vector
#'   (`TRUE`).
#' @examples
#' plan <- drake_plan(x = sample.int(15))
#' cache <- storr::storr_environment() # custom in-memory cache
#' make(plan, cache = cache)
#' config <- drake_config(plan, cache = cache)
#' show_source(x, config)
show_source <- function(target, config, character_only = FALSE) {
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  cache <- config$cache
  meta <- diagnose(target = target, cache = cache, character_only = TRUE)
  prefix <- ifelse(is_encoded_path(target), "File ", "Target ")
  if (meta$imported) {
    message(prefix, target, " was imported.")
  } else {
    command <- gsub("^\\{\n ", "", meta$command)
    command <- gsub(" \n\\}$", "", command)
    message(
      prefix, target, " was build from command:\n  ", target, " = ", command)
  }
}
