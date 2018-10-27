.onAttach <- function(libname, pkgname){
  verbose <- interactive() && (sample.int(n = 10, size = 1) < 1.5)
  f <- ifelse(verbose, invisible, suppressPackageStartupMessages)
  f(drake_tip_message())
  invisible()
}

.onLoad <- function(libname, pkgname) {
  if (file.exists(".RData")){
    warning(
      "Auto-saved workspace file '.RData' detected. ",
      "This is bad for reproducible code. ",
      "You can remove it with unlink(\".RData\").",
      call. = FALSE
    )
  }
  invisible()
}
