.onAttach <- function(libname, pkgname) {
  verbose <- interactive()
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
      "{drake} is superseded: https://books.ropensci.org/targets/drake.html",
      "Consider {targets} instead: https://docs.ropensci.org/targets/",
      "Effective 2021-01-21"
    )
  )
  if (requireNamespace("cli", quietly = TRUE)) {
    tips <- lapply(tips, function(x) {
      paste(cli::col_blue(cli::symbol$bullet), x)
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
