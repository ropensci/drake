.onAttach <- function(libname, pkgname){
  if (interactive() & runif(1) < 0.1) packageStartupMessage(drake_tip())
  invisible()
}

.onLoad <- function(libname, pkgname) {
  if(file.exists(".RData")) 
    warning("Auto-saved workspace file '.RData' detected. ",
      "This is bad for reproducible code. ",
      "You can remove it with unlink('.RData').")
  invisible()
}
