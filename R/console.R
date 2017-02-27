console = function(action = c("build", "import"), messsage){
  action = match.arg(action)
  action = ifelse(action == "build", 
    color(action, "forestgreen"), color(action, "dodgerblue3"))
  if(nchar(message) >= 50) message = paste0(substr(message, 1, 47), "...")
  cat(action, " ", name, "\n", sep = "")
}

color = function(x, color){
  if(is.null(color)) x
  else make_style(color)(x)
}
