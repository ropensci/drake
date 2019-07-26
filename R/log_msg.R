log_msg <- function(
  ...,
  target = character(0),
  config,
  tier = 2L,
  color = "default"
) {
  assert_config_not_plan(config)
  drake_log(..., target = target, config = config)
  if (is.null(config$verbose) || as.integer(config$verbose) < tier) {
    return()
  }
  if (tier > 1L) {
    if (!is.null(.pkg_envir$spinner)) {
      .pkg_envir$spinner$spin()
    }
    return()
  }
  msg <- c(...)
  hex <- text_color(color)
  if (!is.null(hex) && requireNamespace("crayon", quietly = TRUE)) {
    msg[1] <- crayon::make_style(hex)(msg[1])
  }
  if (config$verbose > 1L) {
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

drake_log <- function(..., target = character(0), config) {
  if (is.null(config$console_log_file)) {
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
  write(x = msg, file = config$console_log_file, append = TRUE)
  invisible()
}
