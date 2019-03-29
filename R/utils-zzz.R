.onAttach <- function(libname, pkgname) {
  verbose <- interactive() && (sample.int(n = 10, size = 1) < 1.5)
  f <- ifelse(verbose, invisible, suppressPackageStartupMessages)
  f(drake_tip_message())
  invisible()
}

.onLoad <- function(libname, pkgname) {
  if (file.exists(".RData")) {
    warning(
      "Auto-saved workspace file '.RData' detected. ",
      "This is bad for reproducible code. ",
      "You can remove it with unlink(\".RData\"). ",
      "To avoid generating '.RData' files, ",
      "start your session with 'R --no-save'",
      "or disable the saving of workspace images ",
      "in the RStudio IDE settings.",
      call. = FALSE
    )
  }
  invisible()
}

.pkg_envir <- new.env(parent = emptyenv())

if (requireNamespace("cli", quietly = TRUE)) {
  .pkg_envir$spinner <- cli::make_spinner()
} else {
  # nocov start
  message(
    "Install the ", shQuote("cli"), " package to show a console spinner",
    "for make(verbose = 2)."
  )
  # nocov end
}
