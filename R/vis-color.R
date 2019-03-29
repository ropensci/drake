# Below, the colors from "target" through
# "fail" are for the console. The rest
# are for graph visualizations.
colors <- c(
  default = "dodgerblue3",
  target = "green3",
  retry = "#9400d3",
  missing = "#9400d3",
  fail = "red",
  up_to_date = "forestgreen",
  outdated = "#000000",
  failed = "#aa0000",
  import_node = "dodgerblue3",
  missing_node = "darkorchid3",
  running = "#ff7221",
  other = "#888888"
)

# Show drake's color palette.
drake_palette_ <- function() {
  assert_pkg("crayon")
  for (i in seq_along(colors)) {
    message(crayon::make_style(colors[i])(names(colors)[i]))
  }
}

color_of <- Vectorize(function(x) {
  available <- x %in% names(colors)
  if (!available) {
    x <- "other"
  }
  col2hex(colors[x])
},
"x", USE.NAMES = FALSE)

# copied from the gtools package
col2hex <- function(cname) {
  assert_pkg("grDevices")
  col_mat <- grDevices::col2rgb(cname)
  grDevices::rgb(
    red = col_mat[1, ] / 255,
    green = col_mat[2, ] / 255,
    blue = col_mat[3, ] / 255
  )
}
