logger <- function(verbose, file = NULL) {
  verbose <- as.integer(verbose)
  refclass_logger$new(verbose = verbose, file = file)
}

refclass_logger <- methods::setRefClass(
  Class = "refclass_logger",
  fields = c("verbose", "file"),
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
        cli_msg(msg, cli_fun = cli::cli_alert_success)
      }
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
  write(x = msg, file = file, append = TRUE)
  invisible()
}

cli_msg <- function(..., cli_fun = cli::cli_alert_info) {
  if (.pkg_envir$has_cli) {
    cli_fun(crop_text(paste(...), width = getOption("width") - 2L))
  } else {
    message(crop_text(paste(...))) # nocov
  }
}

target_msg <- function(target) {
  if (.pkg_envir$has_cli) {
    UseMethod("target_msg")
  } else {
    message(paste(class(target), target)) # nocov
  }
}

crop_text <- Vectorize(function(x, width = getOption("width")) {
  if (nchar(x) > width) {
    x <- paste0(substr(x, 1, width - 3), "...")
  }
  x
},
"x", USE.NAMES = FALSE)


target_msg.aggregate <- function(target) {
  symbol <- cli::col_green(cli::symbol$stop)
  msg <- paste(symbol, "aggregate", target)
  message(msg)
}

target_msg.cancel <- function(target) {
  symbol <- cli::col_yellow(cli::symbol$stop)
  msg <- paste(symbol, "cancel", target)
  message(msg)
}

target_msg.dynamic <- function(target) {
  symbol <- cli::col_green(cli::symbol$play)
  msg <- paste(symbol, "dynamic", target)
  message(msg)
}

target_msg.fail <- function(target) {
  symbol <- cli::col_red(cli::symbol$cross)
  msg <- paste(symbol, "fail", target)
  message(msg)
}

target_msg.recover <- function(target) {
  symbol <- cli::col_green(cli::symbol$tick)
  msg <- paste(symbol, "recover", target)
  message(msg)
}

target_msg.retry <- function(target) {
  symbol <- cli::col_yellow(cli::symbol$warning)
  msg <- paste(symbol, "retry", target)
  message(msg)
}

target_msg.subtarget <- function(target) {
  symbol <- cli::col_green(cli::symbol$pointer)
  msg <- paste(symbol, "target", target)
  message(msg)
}

target_msg.target <- function(target) {
  symbol <- cli::col_green(cli::symbol$play)
  msg <- paste(symbol, "target", target)
  message(msg)
}
