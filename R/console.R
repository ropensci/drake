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

console_preprocess <- function(text, pattern, config){
  if (config$verbose < 2){
    return()
  }
  finish_console(text = text, pattern = pattern, config = config)
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
  custom_triggers <- "trigger" %in% colnames(plan) ||
    !identical(config$trigger, trigger())
  if (!any_attempted && !custom_triggers && !config$skip_imports){
    console_all_up_to_date(config = config)
    return(invisible())
  }
  if (config$skip_imports){
    console_skipped_imports(config = config)
  }
  if (custom_triggers){
    console_custom_triggers(config)
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

console_custom_triggers <- function(config){
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
  if (is.null(config$verbose) || config$verbose < 1){
    return(invisible())
  }
  msg <- crop_text(x = text) %>%
   color_grep(pattern = pattern, color = color_of(pattern))
  drake_message(msg, config = config)
}

drake_message <- function(..., config){
  text <- paste0(...)
  if (requireNamespace("crayon", quietly = TRUE)){
    text <- crayon::strip_style(text)
  }
  if (!is.null(config$console_log_file)){
    write(
      x = text,
      file = config$console_log_file,
      append = TRUE
    )
  }
  message(..., sep = "")
}

drake_warning <- function(..., config){
  text <- paste0("Warning: ", ...)
  if (requireNamespace("crayon", quietly = TRUE)){
    text <- crayon::strip_style(text)
  }
  if (!is.null(config$console_log_file)){
    write(
      x = text,
      sep = "",
      file = config$console_log_file,
      append = TRUE
    )
  }
  warning(..., call. = FALSE)
}

drake_error <- function(..., config){
  text <- paste0("Error: ", ...)
  if (requireNamespace("crayon", quietly = TRUE)){
    text <- crayon::strip_style(text)
  }
  if (!is.null(config$console_log_file)){
    write(
      x = text,
      sep = "",
      file = config$console_log_file,
      append = TRUE
    )
  }
  stop(..., call. = FALSE)
}

crop_text <- Vectorize(function(x, width = getOption("width")) {
  if (nchar(x) > width){
    x <- paste0(substr(x, 1, width - 3), "...")
  }
  x
},
"x", USE.NAMES = FALSE)

crop_lines <- function(x, n = 10) {
  if (length(x) > n){
    x <- x[1:(n - 1)]
    x[n] <- "..."
  }
  x
}

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

#' @title Show how a target/import was produced.
#' @description Show the command that produced a target
#'   or indicate that the object or file was imported.
#' @export
#' @param target symbol denoting the target or import
#'   or a character vector if character_only is `TRUE`.
#' @param config a [drake_config()] list
#' @param character_only logical, whether to interpret
#'   `target` as a symbol (`FALSE`) or character vector
#'   (`TRUE`).
#' @examples
#' \dontrun{
#' plan <- drake_plan(x = rnorm(15))
#' make(plan)
#' config <- drake_config(plan)
#' show_source(x, config)
#' show_source(rnorm, config)
#' }
show_source <- function(target, config, character_only = FALSE){
  if (!character_only){
    target <- as.character(substitute(target))
  }
  cache <- config$cache
  meta <- diagnose(target = target, cache = cache, character_only = TRUE)
  prefix <- ifelse(is_file(target), "File ", "Object ")
  if (meta$imported){
    message(prefix, target, " was imported.")
  } else {
    command <- gsub("^\\{\n ", "", meta$command)
    command <- gsub(" \n\\}$", "", command)
    message(
      prefix, target, " was build from command:\n  ", target, " = ", command)
  }
}
