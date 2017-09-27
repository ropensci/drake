#' @title Function palette
#' @export
#' @description show color palette for drake.
#' Your console must have the crayon package enabled.
#' @examples
#' drake_palette()
drake_palette <- function(){
  out <- lapply(
    sort(names(colors)),
    function(x) {
      color(x, color = colors[x])
    }
  )
  out <- paste(out, collapse = "\n")
  cat(out, "\n")
}

color <- function(x, color) {
  if (is.null(color)){
    x
  } else {
    crayon::make_style(color)(x)
  }
}

colors <- c(
  target = "forestgreen",
  import = "dodgerblue3",
  missing = "darkorchid3",
  check = "steelblue3",
  load = "orange3",
  unload = "red3",
  outdated = "#aa0000",
  in_progress = "#ff7221",
  other = "#888888"
)

color_of <- Vectorize(function(x){
  available <- x %in% names(colors)
  if (!available) {
    x <- "other"
  }
  col2hex(colors[x])
},
"x", USE.NAMES = FALSE)

color_grep <- function(text, pattern, color){
  colored_pattern <- color(x = pattern, color = color)
  gsub(
    pattern = paste0("^", pattern, " "),
    replacement = paste0(colored_pattern, " "),
    x = text
  )
}

# copied from the gtools package
col2hex <- function(cname){
  colMat <- grDevices::col2rgb(cname)
  grDevices::rgb(
    red = colMat[1, ] / 255,
    green = colMat[2, ] / 255,
    blue = colMat[3, ] / 255
  )
}

palette <- function(){
  out <- lapply(
    names(colors),
    function(x) {
      color(x, color = colors[x])
    }
  )
  out <- paste(out, collapse = "\n")
  cat(out, "\n")
}



shape_of <- Vectorize(function(x){
  switch(x,
    object = "dot",
    file = "square",
    funct = "triangle",
    "dot"
  )
},
"x", USE.NAMES = FALSE)
