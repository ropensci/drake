console_length <- 80

console <- function(imported, target, config) {
  if (!config$verbose) {
    return()
  }
  if (is.na(imported)) {
    message <- "missing"
  } else if (imported) {
    message <- "import"
  } else {
    message <- "target"
  }
  paste(message, target) %>%
    finish_console(message = message)
}

console_many_targets <- function(
  targets, message, config, color = color_of(message)
){
  if (!config$verbose) {
    return(invisible())
  }
  n <- length(targets)
  if (n < 1){
    return(invisible())
  }
  paste0(
    message,
    " ", n, " item",
    ifelse(n == 1, "", "s"),
    ": ",
    paste(targets, collapse = ", ")
  ) %>%
    finish_console(message = message)
}

finish_console <- function(text, message){
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
