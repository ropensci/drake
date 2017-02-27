.onLoad <- function(libname, pkgname) {
  if(file.exists(".RData")) 
    warning("Auto-saved workspace file '.RData' detected. ",
      "This is bad for reproducible command. ",
      "You can remove it with unlink('.RData').")
  invisible()
}
