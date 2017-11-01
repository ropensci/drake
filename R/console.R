console_length <- 80

console <- function(imported, target, config) {
  if (is.na(imported)) {
    message <- "missing"
  } else if (imported) {
    message <- "import"
  } else {
    message <- "target"
  }
  paste(message, target) %>%
    finish_console(message = message, verbose = config$verbose)
}

console_cache <- function(path, verbose){
  if (!length(path)){
    path <- default_cache_path()
  }
  paste("cache", path) %>%
    finish_console(message = "cache", verbose = verbose)
}

console_many_targets <- function(
  targets, message, config, color = color_of(message), type = "item"
){
  n <- length(targets)
  if (n < 1){
    return(invisible())
  }
  paste0(
    message,
    " ", n, " ", type,
    ifelse(n == 1, "", "s"),
    ": ",
    paste(targets, collapse = ", ")
  ) %>%
    finish_console(message = message, verbose = config$verbose)
}

console_parLapply <- function(config){
  finish_console(text = "load parallel socket cluster", message = "load",
    verbose = config$verbose)
}

console_retry <- function(target, retries, config){
  if (retries <= config$retries){
    text <- paste0("retry ", target, ": ", retries, " of ", config$retries)
    finish_console(text = text, message = "retry", verbose = config$verbose)
  }
}

console_up_to_date <- function(config){
  any_attempted <- length(config$cache$list(namespace = "target_attempts"))
  if (config$verbose && !any_attempted){
    color("All targets are already up to date.\n", colors["target"]) %>%
      cat
  }
}

finish_console <- function(text, message, verbose){
  if (!verbose){
    return(invisible())
  }
  crop_text(x = text, length = console_length) %>%
    color_grep(pattern = message, color = color_of(message)) %>%
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
