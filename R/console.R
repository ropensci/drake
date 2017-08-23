console <- function(imported, target, config){
  if (!config$verbose){
    return()
  }
  if (is.na(imported)){
    action <- color("could not find", "darkorchid3")
  } else if (imported){
    action <- color("import", "dodgerblue3")
  } else{
    action <- color("build", "forestgreen")
  }
  if (nchar(target) > 50){
    target <- paste0(substr(target, 1, 47), "...")
  }
  cat(action, " ", target, "\n", sep = "")
}

color <- function(x, color){
  if (is.null(color)){
    x
  } else{
    make_style(color)(x)
  }
}
