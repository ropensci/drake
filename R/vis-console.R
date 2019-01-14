console <- function(imported, target, config) {
  if (is.na(imported)) {
    console_missing(target = target, config = config)
  } else if (imported) {
    console_import(target = target, config = config)
  } else {
    console_target(target = target, config = config)
  }
}

console_generic <- function(
  target,
  config,
  cutoff = 1,
  pattern = "target",
  tail = ""
) {
  if (config$verbose < cutoff) {
    return()
  }
  text <- display_keys(target, config)
  text <- paste0(pattern, " ", text, tail)
  finish_console(text = text, pattern = pattern, config = config)

}

console_missing <- function(target, config) {
  console_generic(target, config, 3L, "missing")
}

console_import <- function(target, config) {
  console_generic(target, config, 4L, "import")
}

console_skip <- function(target, config) {
  console_generic(target, config, 4L, "skip")
}

console_store <- function(target, config) {
  console_generic(target, config, 6L, "store")
}

console_target <- function(target, config) {
  console_generic(target, config, 1L, "target")
}

console_time <- function(target, meta, config) {
  tol <- 5L
  if (config$verbose < tol) {
    return()
  }
  if (requireNamespace("lubridate", quietly = TRUE)) {
    exec <- round(lubridate::dseconds(meta$time_command$elapsed), 3)
    total <- round(lubridate::dseconds( meta$time_build$elapsed), 3)
    tail <- paste("", exec, "(exec)", total, "(total)")
  } else {
    tail <- " (install lubridate)" # nocov
  }
  console_generic(target, config, tol, "time", tail = tail)
}

console_cache <- function(config) {
  if (config$verbose < 2) {
    return()
  }
  if (is.null(config$cache_path)) {
    config$cache_path <- default_cache_path()
  }
  out <- paste("cache", config$cache_path)
  finish_console(out, pattern = "cache", config = config)
}

console_preprocess <- function(text, config) {
  if (!length(config$verbose) || config$verbose < 2) {
    return()
  }
  finish_console(
    text = text,
    pattern = strsplit(text, " ")[[1]][1],
    config = config
  )
}

console_many_targets <- function(
  targets, pattern, config, color = color_of(pattern), type = "item"
) {
  if (config$verbose < 2) {
    return()
  }
  n <- length(targets)
  if (n < 1) {
    return(invisible())
  }
  targets <- display_keys(targets, config)
  out <- paste0(
    pattern,
    " ", n, " ", type,
    ifelse(n == 1, "", "s"),
    ": ",
    paste(targets, collapse = ", ")
  )
  finish_console(out, pattern = pattern, config = config)
}

console_parLapply <- function(config) { # nolint
  text <- paste("load parallel socket cluster with", config$jobs, "workers")
  finish_console(text = text, pattern = "load", config = config)
}

console_retry <- function(target, error, retries, config) {
  if (retries <= config$retries) {
    text <- paste0("retry ", target, ": ", retries, " of ", config$retries)
    finish_console(text = text, pattern = "retry", config = config)
  }
}

console_up_to_date <- function(config) {
  if (!config$verbose) {
    return(invisible())
  } else {
    out <- color("All targets are already up to date.", colors["target"])
    drake_message(out, config = config)
  }
}

console_edge_cases <- function(config) {
  if (config$verbose < 1L) {
    return()
  }
  if (config$skip_imports) {
    console_skipped_imports(config = config)
  }
  custom_triggers <- "trigger" %in% colnames(config$plan) ||
    !identical(config$trigger, trigger())
  if (custom_triggers) {
    console_custom_triggers(config)
  }
}

console_skipped_imports <- function(config) {
  out <- color(
    paste(
      "Skipped the imports.",
      "If some imports are not already cached, targets could be out of date."
    ),
    colors["trigger"]
  )
  drake_message(out, config = config)
}

console_custom_triggers <- function(config) {
  out <- color(
    paste(
      "Used non-default triggers.",
      "Some targets may not be up to date."
    ),
    colors["trigger"]
  )
  drake_message(out, config = config)
}

console_persistent_workers <- function(config) {
  if (config$verbose < 2) {
    return()
  }
  finish_console(
    text = paste("launch", config$jobs, "persistent workers + master"),
    pattern = "launch",
    config = config
  )
}

finish_console <- function(text, pattern, config) {
  if (is.null(config$verbose) || config$verbose < 1) {
    return(invisible())
  }
  msg <- crop_text(x = text)
  msg <- color_grep(msg, pattern = pattern, color = color_of(pattern))
  drake_message(msg, config = config)
}

drake_log <- function(..., prefix, config) {
  text <- paste0(prefix, ...)
  if (requireNamespace("crayon", quietly = TRUE)) {
    text <- crayon::strip_style(text)
  }
  if (!is.null(config$console_log_file)) {
    write(
      x = text,
      file = config$console_log_file,
      append = TRUE
    )
  }
  invisible()
}

drake_log_message <- function(..., config) {
  drake_log(..., prefix = "", config = config)
}

drake_log_warning <- function(..., config) {
  drake_log(..., prefix = "Warning: ", config = config)
}

drake_log_error <- function(..., config) {
  drake_log(..., prefix = "Error: ", config = config)
}

drake_message <- function(..., config) {
  drake_log_message(..., config = config)
  message(..., sep = "")
}

drake_warning <- function(..., config) {
  drake_log_warning(..., config = config)
  warning(..., call. = FALSE)
}

drake_error <- function(..., config) {
  drake_log_error(..., config = config)
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
