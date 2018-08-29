# Below, the colors from "target" through
# "fail" are for the console. The rest
# are for graph visualizations.
colors <- c(
  target = "green3",
  import = "dodgerblue3",
  missing = "darkorchid3",
  cache = "skyblue1",
  check = "skyblue1",
  analyze = "skyblue1",
  construct = "skyblue1",
  launch = "#ff9933",
  load = "#ff9933",
  unload = "#ff7221",
  trigger = "maroon",
  skip = "skyblue1",
  retry = "forestgreen",
  fail = "red",
  up_to_date = "forestgreen",
  outdated = "#000000",
  failed = "#aa0000",
  import_node = "dodgerblue3",
  missing_node = "darkorchid3",
  in_progress = "#ff7221",
  other = "#888888"
)

#' @title Show drake's color palette.
#' @export
#' @description This function is
#' used in both the console and graph visualizations.
#' Your console must have the crayon package enabled.
#' @details This palette applies to console output
#' (internal functions `console()` and
#' `console_many_targets()`) and the node colors
#' in the graph visualizations.
#' So if you want to contribute improvements to the palette,
#' please both `drake_palette()` and
#' `visNetwork::visNetwork(nodes = legend_nodes())`
#' @return There is a console message,
#'   but the actual return value is `NULL`.
#' @examples
#' # Show drake's color palette as text.
#' drake_palette()
#' # Show part of the palette as an interactive visNetwork graph.
#' # These are the nodes in the legend of the graph visualizations.
#' \dontrun{
#' # visNetwork::visNetwork(nodes = legend_nodes()) # nolint
#' }
drake_palette <- function(){
  out <- lapply(
    sort(names(colors)),
    function(x) {
      color(x, color = colors[x])
    }
  )
  out <- paste(out, collapse = "\n")
  message(out)
}

color <- function(x, color) {
  if (is.null(color) || !requireNamespace("crayon", quietly = TRUE)){
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
  assert_pkg("grDevices")
  col_mat <- grDevices::col2rgb(cname)
  grDevices::rgb(
    red = col_mat[1, ] / 255,
    green = col_mat[2, ] / 255,
    blue = col_mat[3, ] / 255
  )
}
