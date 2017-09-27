color <- function(x, color) {
  if (is.null(color)){
    x
  } else {
    crayon::make_style(color)(x)
  }
}

color_of <- Vectorize(function(x){
  switch(x,
    target = "forestgreen",
    import = "dodgerblue3",
    missing = "darkorchid3",
    check = "steelblue3",
    load = "orange3",
    unload = "red3",
    outdated = "#aa0000",
    in_progress = "#ff7221",
    "#888888"
  ) %>%
    gplots::col2hex()
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

shape_of <- Vectorize(function(x){
  switch(x,
    object = "dot",
    file = "square",
    funct = "triangle",
    "dot"
  )
},
"x", USE.NAMES = FALSE)
