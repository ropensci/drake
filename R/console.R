console_length <- 50

console <- function(imported, target, config) {
  if (!config$verbose)
    return()
  if (is.na(imported))
    action <- color("could not find", color_of("missing"))
  else if (imported)
    action <- color("import", color_of("import"))
  else
    action <- color("target", color_of("target"))
  out <- paste(action, target) %>%
    crop_text(length = console_length)
  cat(out, "\n", sep = "")
}

console_many_targets <- function(
  targets, message, config, color = color_of(message)
){
  if (!config$verbose) return(invisible())
  n <- length(targets)
  if (n < 1){
    return(invisible())
  }
  out <- paste0(
    color(message, color), " ", n, " item",
    ifelse(n == 1, "", "s"), ": ",
    paste(targets, collapse = ", ")
  ) %>%
    crop_text(length = console_length)
  cat(out, "\n", sep = "")
}

crop_text <- Vectorize(function(x, length = 50) {
  if (nchar(x) > length)
    x <- paste0(substr(x, 1, length - 3), "...")
  x
},
"x", USE.NAMES = FALSE)
