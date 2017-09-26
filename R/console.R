console_length <- 50

console <- function(imported, target, config) {
  if (!config$verbose)
    return()
  if (is.na(imported))
    action <- color("could not find", "darkorchid3")
  else if (imported)
    action <- color("import", "dodgerblue3")
  else
    action <- color("target", "forestgreen")
  out <- paste(action, target) %>%
    crop_text(length = console_length)
  cat(out, "\n", sep = "")
}

console_many_targets <- function(
  targets, message, config, color = "slateblue2"
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

color <- function(x, color) {
  if (is.null(color)){
    x
  } else {
    crayon::make_style(color)(x)
  }
}

crop_text <- Vectorize(function(x, length = 50) {
  if (nchar(x) > length)
    x <- paste0(substr(x, 1, length - 3), "...")
  x
},
"x", USE.NAMES = FALSE)
