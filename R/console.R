console = function(action = c("build", "import"), target){
  action = match.arg(action)
  action = ifelse(action == "build", 
    color(action, "forestgreen"), color(action, "dodgerblue3"))
  if(nchar(target) >= 50) target = paste0(substr(target, 1, 47), "...")
  cat(action, " ", target, "\n", sep = "")
}

color = function(x, color){
  if(is.null(color)) x
  else make_style(color)(x)
}
