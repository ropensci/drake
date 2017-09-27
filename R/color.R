colors <- c(
  target = "forestgreen",
  import = "dodgerblue3",
  missing = "darkorchid3",
  check = "skyblue1",
  load = "gold",
  unload = "#ffa500",
  outdated = "#aa0000",
  in_progress = "#ff7221",
  other = "#888888"
)

#' @title Function palette
#' @export
#' @description show color palette for drake.
#' Used in both the console and \code{\link{plot_graph}()}
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
  col_mat <- grDevices::col2rgb(cname)
  grDevices::rgb(
    red = col_mat[1, ] / 255,
    green = col_mat[2, ] / 255,
    blue = col_mat[3, ] / 255
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
