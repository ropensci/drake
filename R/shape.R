shapes <- c(
  object = "dot",
  file = "square",
  funct = "triangle",
  other = "dot"
)

shape_of <- Vectorize(function(x){
  available <- x %in% names(shapes)
  if (!available) {
    x <- "other"
  }
  shapes[x]
},
"x", USE.NAMES = FALSE)
