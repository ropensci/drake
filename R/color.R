color = function(x, color){
  if(is.null(color)) x
  else make_style(color)(x)
}
