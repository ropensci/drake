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
  tips <- list(
    c(
      "Interfaces to create large drake plans:",
      "https://books.ropensci.org/drake/dynamic.html, ",
      "https://books.ropensci.org/drake/static.html"
    ),
    c(
      "Use diagnose() to retrieve",
      "errors, warnings, messages, commands, runtimes, etc."
    ),
    "Use drake_example() to download a small drake example workflow.",
    c(
      "Reference website: https://docs.ropensci.org/drake",
      "User manual: https://books.ropensci.org/drake"
    ),
    "drake quickstart: run load_mtcars_example() then make(my_plan)"
  )
  if (requireNamespace("cli", quietly = TRUE)) {
    tips <- lapply(tips, function(x) {
      paste(cli::col_blue(cli::symbol$info), x)
    })
  }
  tips <- soft_wrap(tips)
  sample(tips, 1)
}

.onLoad <- function(libname, pkgname) {
  warn_rdata()
  invisible()
  .pkg_envir[["has_cli"]] <- requireNamespace("cli", quietly = TRUE)
  .pkg_envir[["has_progress"]] <- requireNamespace("progress", quietly = TRUE)
  .pkg_envir[["on_windows"]] <- this_os() == "windows"
}

warn_rdata <- function() {
  if (!file.exists(".RData")) {
    return()
  }
  warn0(
    "Auto-saved .RData file detected. Remove it to enhance reproducibility."
  )
}

.pkg_envir <- new.env(parent = emptyenv())
