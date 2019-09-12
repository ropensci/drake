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

    "Check out the reference website https://docs.ropensci.org/drake
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

.pkg_envir <- new.env(parent = emptyenv())
