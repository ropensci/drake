console <- function(imported, target, config) {
  if (is.na(imported)) {
    console_missing(target = target, config = config)
  } else if (imported) {
    console_import(target = target, config = config)
  } else {
    console_target(target = target, config = config)
  }
}

console_missing <- function(target, config){
  if (config$verbose < 3){
    return()
  }
  pattern <- "missing"
  text <- target
  if (is_file(target)){
    text <- paste0("file ", text)
  }
  text <- paste(pattern, text)
  finish_console(text = text, pattern = pattern, config = config)
}

console_import <- function(target, config){
  if (config$verbose < 4){
    return()
  }
  pattern <- "import"
  text <- target
  if (is_file(target)){
    text <- paste0("file ", text)
  }
  text <- paste(pattern, text)
  finish_console(text = text, pattern = pattern, config = config)
}

console_skip <- function(target, config){
  if (config$verbose < 4){
    return()
  }
  pattern <- "skip"
  text <- target
  if (is_file(target)){
    text <- paste0("file ", text)
  }
  text <- paste(pattern, text)
  finish_console(text = text, pattern = pattern, config = config)
}

console_target <- function(target, config){
  pattern <- "target"
  text <- target
  if (is_file(target)){
    text <- paste0("file ", text)
  }
  text <- paste("target", text)
  trigger <- get_trigger(target = target, config = config)
  if (trigger != "any"){
    trigger <- get_trigger(target = target, config = config)
    trigger_text <- color(x = "trigger", color = color_of("trigger"))
    text <- paste0(text, ": ", trigger_text, " \"", trigger, "\"")
  }
  finish_console(text = text, pattern = pattern, config = config)
}

console_cache <- function(config){
  if (config$verbose < 2){
    return()
  }
  if (is.null(config$cache_path)){
    config$cache_path <- default_cache_path()
  }
  paste("cache", config$cache_path) %>%
    finish_console(pattern = "cache", config = config)
}

console_many_targets <- function(
  targets, pattern, config, color = color_of(pattern), type = "item"
){
  if (config$verbose < 2){
    return()
  }
  n <- length(targets)
  if (n < 1){
    return(invisible())
  }
  targets[is_file(targets)] <- paste("file", targets[is_file(targets)])
  paste0(
    pattern,
    " ", n, " ", type,
    ifelse(n == 1, "", "s"),
    ": ",
    paste(targets, collapse = ", ")
  ) %>%
    finish_console(pattern = pattern, config = config)
}

console_parLapply <- function(config){ # nolint
  text <- paste("load parallel socket cluster with", config$jobs, "workers")
  finish_console(text = text, pattern = "load", config = config)
}

console_retry <- function(target, error, retries, config){
  if (retries <= config$retries){
    text <- paste0("retry ", target, ": ", retries, " of ", config$retries)
    finish_console(text = text, pattern = "retry", config = config)
  }
}

console_up_to_date <- function(config){
  if (!config$verbose){
    return(invisible())
  }
  any_attempted <- get_attempt_flag(config = config)
  default_triggers <- using_default_triggers(config)
  if (!any_attempted && default_triggers && !config$skip_imports){
    console_all_up_to_date(config = config)
    return(invisible())
  }
  if (config$skip_imports){
    console_skipped_imports(config = config)
  }
  if (!default_triggers){
    console_nondefault_triggers(config = config)
  }
}

console_all_up_to_date <- function(config){
  color("All targets are already up to date.", colors["target"]) %>%
    drake_message(config = config)
}

console_skipped_imports <- function(config){
  color(
    paste(
      "Skipped the imports.",
      "If some imports are not already cached, targets could be out of date."
    ),
    colors["trigger"]
  ) %>%
    drake_message(config = config)
}

console_nondefault_triggers <- function(config){
  color(
    paste(
      "Used non-default triggers.",
      "Some targets may not be up to date."
    ),
    colors["trigger"]
  ) %>%
    drake_message(config = config)
}

console_persistent_workers <- function(config){
  if (config$verbose < 2){
    return()
  }
  finish_console(
    text = paste("launch", config$jobs, "persistent workers + master"),
    pattern = "launch",
    config = config
  )
}

finish_console <- function(text, pattern, config){
  if (config$verbose < 1){
    return(invisible())
  }
  msg <- crop_text(x = text)
  if (is.null(config$console)){
    msg <- color_grep(
      text = msg,
      pattern = pattern,
      color = color_of(pattern)
    )
  }
  drake_message(msg, config = config)
}

drake_message <- function(..., config){
  if (is.null(config$console)){
    message(..., sep = "")
  } else {
    write(x = paste0(...), file = config$console, append = TRUE)
  }
}

drake_warning <- function(..., config){
  if (!is.null(config$console)){
    write(
      x = paste0("Warning: ", ...),
      sep = "",
      file = config$console,
      append = TRUE
    )
  }
  warning(..., call. = FALSE)
}

drake_error <- function(..., config){
  if (!is.null(config$console)){
    write(
      x = paste0("Error: ", ...),
      sep = "",
      file = config$console,
      append = TRUE
    )
  }
  stop(..., call. = FALSE)
}

crop_text <- Vectorize(function(x, width = getOption("width")) {
  if (nchar(x) > width)
    x <- paste0(substr(x, 1, width - 3), "...")
  x
},
"x", USE.NAMES = FALSE)

multiline_message <- function(x) {
  n <- 30
  if (length(x) > n){
    x <- c(x[1:(n - 1)], "...")
  }
  paste0("  ", x) %>% paste(collapse = "\n")
}

#' @title Default verbosity for `drake`
#' @description Set with `pkgconfig`: for example,
#'   `pkgconfig::set_config("drake::verbose" = 2)`.
#' @export
#' @keywords internal
#' @return a logical or integer with the value of
#'   the default `verbose` argument to `drake` functions.
default_verbose <- function(){
  default <- pkgconfig::get_config("drake::verbose")
  ifelse(!length(default), 1, default)
}
