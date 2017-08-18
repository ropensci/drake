console <- function(imported, target, config) {
  if (!config$verbose)
    return()
  if (is.na(imported))
    action <- color("could not find", "darkorchid3")
  else if (imported)
    action <- color("import", "dodgerblue3")
  else
    action <- color("build", "forestgreen")
  target <- crop_text(target, length = 50)
  cat(action, " ", target, "\n", sep = "")
}

console_parallel_stage <- function(targets, config){
  if (!config$verbose) return()
  n <- length(targets)
  cat(color("check", "slateblue2"), " ", n, " item",
    ifelse(n == 1, "", "s"), "\n", sep = "")
}

color <- function(x, color) {
  if (is.null(color))
    x else make_style(color)(x)
}

crop_text <- Vectorize(function(x, length = 50) {
  if (nchar(x) > length)
    x <- paste0(substr(x, 1, length - 3), "...")
  x
},
"x", USE.NAMES = FALSE)
