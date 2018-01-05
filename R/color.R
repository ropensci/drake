# Below, the colors from "target" through
# "fail" are for the console. The rest
# are for vis_drake_graph().
colors <- c(
  target = "green3",
  import = "dodgerblue3",
  missing = "darkorchid3",
  cache = "skyblue1",
  check = "skyblue1",
  connect = "skyblue1",
  load = "#ff9933",
  unload = "#ff7221",
  trigger = "maroon",
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
#' used in both the console and \code{\link{vis_drake_graph}()}
#' Your console must have the crayon package enabled.
#' @details This palette applies to console output
#' (internal functions \code{console()} and
#' \code{console_many_targets()}) and the node colors
#' in \code{\link{vis_drake_graph}()}.
#' So if you want to contribute improvements to the palette,
#' please both \code{drake_palette()} and
#' \code{visNetwork::visNetwork(nodes = \link{legend_nodes}())}
#' @return There is a console message,
#' but the actual return value is \code{NULL}.
#' @examples
#' # Show drake's color palette as text.
#' drake_palette()
#' # Show part of the palette as an interactive visNetwork graph.
#' # These are the nodes in the legend of vis_drake_graph().
#' \dontrun{
#' visNetwork::visNetwork(nodes = legend_nodes())
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
