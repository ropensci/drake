console_length <- 80

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
  pattern <- "missing"
  text <- paste(pattern, target)
  finish_console(text = text, pattern = pattern, verbose = config$verbose)
}

console_import <- function(target, config){
  pattern <- "import"
  text <- paste(pattern, target)
  finish_console(text = text, pattern = pattern, verbose = config$verbose)
}

console_target <- function(target, config){
  pattern <- "target"
  text <- paste("target", target)
  if ("trigger" %in% colnames(config$plan)){
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
  finish_console(text = "load parallel socket cluster", pattern = "load",
    verbose = config$verbose)
}

console_retry <- function(target, retries, config){
  if (retries <= config$retries){
    text <- paste0("retry ", target, ": ", retries, " of ", config$retries)
    finish_console(text = text, pattern = "retry", verbose = config$verbose)
  }
}

console_up_to_date <- function(config){
  any_attempted <- length(config$cache$list(namespace = "attempts"))
  no_triggers <- is.null(config$plan$trigger) ||
    all(config$plan$trigger == "any")
  if (config$verbose && !any_attempted && no_triggers){
    color("All targets are already up to date.\n", colors["target"]) %>%
      cat
  } else if (!no_triggers){
    color(
      paste(
        "Used non-default triggers.",
        "Some targets may be not be up to date.\n"),
      colors["trigger"]) %>%
      cat
  }
}

finish_console <- function(text, pattern, verbose){
  if (!verbose){
    return(invisible())
  }
  crop_text(x = text, length = console_length) %>%
    color_grep(pattern = pattern, color = color_of(pattern)) %>%
    cat("\n", sep = "")
}

crop_text <- Vectorize(function(x, length = 50) {
  if (nchar(x) > length)
    x <- paste0(substr(x, 1, length - 3), "...")
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
