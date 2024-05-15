logger <- function(verbose, file = NULL) {
  verbose <- as.integer(verbose)
  progress_bar <- NULL
  if (.pkg_envir$has_progress) {
    progress_bar <- progress::progress_bar$new(
      format = "targets [:bar] :percent",
      show_after = 0
    )
  # nocov start
  } else if (verbose == 2L) {
     # Covered if we run tests without the progress package.
     # Part of https://github.com/ropensci/drake/blob/main/inst/testing/cran-checklist.md # nolint
    cli_msg(
      "Install the progress package to see a progress bar when verbose = 2."
    )
  }
  # nocov end
  out <- refclass_logger$new(
    verbose = verbose,
    file = file,
    progress_bar = progress_bar
  )
  out$progress_index <- 0L
  out
}

refclass_logger <- methods::setRefClass(
  Class = "refclass_logger",
  fields = c(
    "verbose",
    "file",
    "progress_bar",
    "progress_index",
    "progress_total"
  ),
  methods = list(
    disk = function(...) {
      drake_log_file(..., file = .self$file)
    },
    term = function(...) {
      if (.self$verbose == 1L) {
        cli_msg(...)
      }
    },
    target = function(target, action) {
      drake_log_file(target = target, file = .self$file, action)
      if (.self$verbose == 1L) {
        class(target) <- action
        target_msg(target)
      }
    },
    up_to_date = function() {
      msg <- "All targets are already up to date."
      drake_log_file(file = .self$file, msg)
      if (.self$verbose >= 1L) {
        cli_msg(msg, cli_sym = cli::col_green(cli::symbol$tick))
      }
    },
    progress = function(increment = 1L) {
      pb <- .self$progress_bar
      .self$progress_index <- .self$progress_index + increment
      if (!is.null(pb) && .self$verbose == 2L) {
        ratio <- min(1, .self$progress_index / .self$progress_total)
        pb$finished <- FALSE
        pb$update(ratio = ratio)
      }
    },
    terminate_progress = function() {
      pb <- .self$progress_bar
      if (!is.null(pb)) {
        pb$terminate()
      }
    },
    set_progress_total = function(n) {
      .self$progress_total <- n
    },
    inc_progress_total = function(n = 1) {
      .self$progress_total <- .self$progress_total + n
    }
  )
)

drake_log_file <- function(..., target = character(0), file) {
  if (is.null(file)) {
    return()
  }
  msg <- paste(
    Sys.info()["nodename"],
    "|",
    Sys.getpid(),
    "|",
    microtime(),
    "|",
    target,
    "|",
    ...
  )
  write(x = sanitize_message(msg), file = file, append = TRUE)
  invisible()
}

sanitize_message <- function(x) {
  x <- gsub("\t", " ", x)
  x <- gsub("[^[:alnum:] \\.,_]", "", x)
  paste(x, collapse = " ")
}

cli_msg <- function(..., cli_sym = cli::col_blue(cli::symbol$info)) {
  if (.pkg_envir$has_cli) {
    message(paste(cli_sym, ...))
  } else {
    message(paste(...)) # nocov
  }
}

target_msg <- function(target) {
  if (.pkg_envir$has_cli) {
    UseMethod("target_msg")
  } else {
    message(paste(class(target), target)) # nocov
  }
}

#' @export
target_msg.finalize <- function(target) {
  symbol <- cli::col_green(cli::symbol$stop)
  msg <- paste(symbol, "finalize", target)
  message(msg)
}

#' @export
target_msg.cancel <- function(target) {
  symbol <- cli::col_yellow(cli::symbol$stop)
  msg <- paste(symbol, "cancel", target)
  message(msg)
}

#' @export
target_msg.dynamic <- function(target) {
  symbol <- cli::col_green(cli::symbol$play)
  msg <- paste(symbol, "dynamic", target)
  message(msg)
}

#' @export
target_msg.fail <- function(target) {
  symbol <- cli::col_red(cli::symbol$cross)
  msg <- paste(symbol, "fail", target)
  message(msg)
}

#' @export
target_msg.recover <- function(target) {
  symbol <- cli::col_green(cli::symbol$tick)
  msg <- paste(symbol, "recover", target)
  message(msg)
}

#' @export
target_msg.retry <- function(target) {
  symbol <- cli::col_yellow(cli::symbol$warning)
  msg <- paste(symbol, "retry", target)
  message(msg)
}

#' @export
target_msg.subtarget <- function(target) {
  symbol <- cli::col_green(cli::symbol$pointer)
  msg <- paste(symbol, "subtarget", target)
  message(msg)
}

#' @export
target_msg.target <- function(target) {
  symbol <- cli::col_green(cli::symbol$play)
  msg <- paste(symbol, "target", target)
  message(msg)
}
