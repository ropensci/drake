console = function(imported, target, args){
  if(!args$verbose | (imported & !is_file(target))) return()
  if(imported)
    action = color("import file", "dodgerblue3")
  else
    action = color("build", "forestgreen")
  if(nchar(target) > 50) target = paste0(substr(target, 1, 47), "...")
  cat(action, " ", target, "\n", sep = "")
}

color = function(x, color){
  if(is.null(color)) x
  else make_style(color)(x)
}
