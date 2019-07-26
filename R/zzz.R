.onAttach <- function(libname, pkgname) {
  verbose <- interactive() && (sample.int(n = 10, size = 1) < 1.5)
  f <- ifelse(verbose, invisible, suppressPackageStartupMessages)
  f(drake_tip_message())
  invisible()
}

drake_tip_message <- function() {
  packageStartupMessage(drake_tip_())
}

drake_tip_ <- function() {
  tips <- c(
    "A new and improved way to create large drake plans:
     https://ropenscilabs.github.io/drake-manual/plans.html#large-plans",

    "Use diagnose() to retrieve
     errors, warnings, messages, commands, runtimes, etc.",

    "Use drake_example() to download code for a small drake workflow.",

    "Check out the reference website https://ropensci.github.io/drake
     and user manual https://ropenscilabs.github.io/drake-manual.",

    "drake quickstart:
     load_mtcars_example();
     make(my_plan);
     readd(small)"
  )
  tips <- wrap_text(tips)
  sample(tips, 1)
}

wrap_text <- Vectorize(
  function(x) {
    x <- paste(strwrap(x), collapse = "\n")
    unname(x)
  },
  "x"
)

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
    utils::compareVersion(
      as.character(utils::packageVersion("cli")),
      "1.1.0"
    ) >= 0L
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

.pkg_envir <- new.env(parent = emptyenv())
