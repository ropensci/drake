.pkg_envir <- new.env(parent = emptyenv())

.onAttach <- function(libname, pkgname) {
  verbose <- interactive() && (sample.int(n = 10, size = 1) < 1.5)
  f <- ifelse(verbose, invisible, suppressPackageStartupMessages)
  f(drake_tip_message())
  invisible()
}

.onLoad <- function(libname, pkgname) {
  warn_rdata()
  try_spinner()
  invisible()
}

warn_rdata <- function() {
  if (!file.exists(".RData")) {
    return()
  }
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

try_spinner <- function() {
  use_cli <- requireNamespace("cli", quietly = TRUE) &&
    compareVersion(as.character(utils::packageVersion("cli")), "1.1.0") >= 0L
  if (use_cli) {
    .pkg_envir$spinner <- cli::make_spinner()
    return()
  }
  # nocov start
  message(
    "Install the ", shQuote("cli"), " package version 1.1.0 or above ",
    "to show a console spinner for make(verbose = 2)."
  )
  # nocov end
}
