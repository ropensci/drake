logger <- function(verbose, file) {
  spinner <- NULL
  if (verbose > 1L) {
    spinner <- try_spinner()
  }
  refclass_logger$new(
    verbose = verbose,
    file = file,
    spinner = spinner
  )
}

try_spinner <- function() {
  use_cli <- requireNamespace("cli", quietly = TRUE) &&
    utils::compareVersion(
      as.character(utils::packageVersion("cli")),
      "1.1.0"
    ) >= 0L
  if (use_cli) {
    return(cli::make_spinner())
  }
  # nocov start
  message(
    "Install the ", shQuote("cli"), " package version 1.1.0 or above ",
    "to show a console spinner for make(verbose = 2)."
  )
  # nocov end
}

refclass_logger <- methods::setRefClass(
  Class = "refclass_logger",
  fields = c("verbose", "file", "spinner"),
  methods = list(
    minor = function(..., target = character(0)) {
      drake_log_file(..., target = target, file = .self$file)
      drake_log_spin(spinner = .self$spinner, verbose = .self$verbose)
      invisible()
    },
    major = function(..., target = character(0), color = "default") {
      drake_log_file(..., target = target, file = .self$file)
      drake_log_term(
        ...,
        target = target,
        color = color,
        verbose = .self$verbose
      )
      invisible()
    }
  )
)

drake_log_spin <- function(spinner, verbose) {
  if (!is.null(spinner) && verbose >= 1L) {
    spinner$spin()
  }
}

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

drake_log_term <- function(..., target, color, verbose) {
  if (verbose < 1L) {
    return()
  }
  msg <- c(...)
  hex <- text_color(color)
  if (!is.null(hex) && requireNamespace("crayon", quietly = TRUE)) {
    msg[1] <- crayon::make_style(hex)(msg[1])
  }
  if (verbose > 1L) {
    msg[1] <- paste0("\r", msg[1])
  }
  message(crop_text(paste(msg, collapse = " ")))
}

text_color <- Vectorize(function(x) {
  color <- switch(
    x,
    default = "dodgerblue3",
    target = "green3",
    recover = "dodgerblue3",
    retry = "#9400d3",
    missing = "#9400d3",
    fail = "red",
    "#888888"
  )
  col2hex(color)
},
"x", USE.NAMES = FALSE)

# copied from the gtools package
col2hex <- function(cname) {
  assert_pkg("grDevices")
  col_mat <- grDevices::col2rgb(cname)
  grDevices::rgb(
    red = col_mat[1, ] / 255,
    green = col_mat[2, ] / 255,
    blue = col_mat[3, ] / 255
  )
}

crop_text <- Vectorize(function(x, width = getOption("width")) {
  if (nchar(x) > width) {
    x <- paste0(substr(x, 1, width - 3), "...")
  }
  x
},
"x", USE.NAMES = FALSE)
