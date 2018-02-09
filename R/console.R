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
  if (config$verbose < 2){
    return()
  }
  pattern <- "missing"
  text <- paste(pattern, target)
  finish_console(text = text, pattern = pattern, verbose = config$verbose)
}

console_import <- function(target, config){
  if (config$verbose < 3){
    return()
  }
  pattern <- "import"
  text <- paste(pattern, target)
  finish_console(text = text, pattern = pattern, verbose = config$verbose)
}

console_target <- function(target, config){
  pattern <- "target"
  text <- paste("target", target)
  trigger <- get_trigger(target = target, config = config)
  if (trigger != "any"){
    trigger <- get_trigger(target = target, config = config)
    trigger_text <- color(x = "trigger", color = color_of("trigger"))
    text <- paste0(text, ": ", trigger_text, " \"", trigger, "\"")
  }
  finish_console(text = text, pattern = pattern, verbose = config$verbose)
}

console_cache <- function(path, verbose){
  if (!length(path)){
    path <- default_cache_path()
  }
  paste("cache", path) %>%
    finish_console(pattern = "cache", verbose = verbose)
}

console_many_targets <- function(
  targets, pattern, config, color = color_of(pattern), type = "item"
){
  n <- length(targets)
  if (n < 1){
    return(invisible())
  }
  paste0(
    pattern,
    " ", n, " ", type,
    ifelse(n == 1, "", "s"),
    ": ",
    paste(targets, collapse = ", ")
  ) %>%
    finish_console(pattern = pattern, verbose = config$verbose)
}

console_parLapply <- function(config){ # nolint
  text <- paste("load parallel socket cluster with", config$jobs, "workers")
  finish_console(text = text, pattern = "load",
    verbose = config$verbose)
}

console_retry <- function(target, error, retries, config){
  if (retries <= config$retries){
    text <- paste0("retry ", target, ": ", retries, " of ", config$retries)
    finish_console(text = text, pattern = "retry", verbose = config$verbose)
  }
}

console_up_to_date <- function(config){
  if (!config$verbose){
    return(invisible())
  }
  any_attempted <- get_attempt_flag(config = config)
  default_triggers <- using_default_triggers(config)
  if (!any_attempted && default_triggers && !config$skip_imports){
    console_all_up_to_date()
    return(invisible())
  }
  if (config$skip_imports){
    console_skipped_imports()
  }
  if (!default_triggers){
    console_nondefault_triggers()
  }
}

console_all_up_to_date <- function(){
  color("All targets are already up to date.", colors["target"]) %>%
      message
}

console_skipped_imports <- function(){
  color(
    paste(
      "Skipped the imports.",
      "If some imports are not already cached, targets could be out of date."
    ),
    colors["trigger"]
  ) %>%
    message
}

console_nondefault_triggers <- function(){
  color(
    paste(
      "Used non-default triggers.",
      "Some targets may be not be up to date."
    ),
    colors["trigger"]
  ) %>%
    message
}

finish_console <- function(text, pattern, verbose){
  if (!verbose){
    return(invisible())
  }
  crop_text(x = text) %>%
    color_grep(pattern = pattern, color = color_of(pattern)) %>%
    message(sep = "")
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
