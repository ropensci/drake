# Functions borrowed directly from codetools to deal with
# nested replacement functions:
# https://github.com/cran/codetools/blob/9bac1daaf19a36bd03a2cd7d67041893032e7a04/R/codetools.R#L302-L365 # nolint
# The code is following the subset assignment section
# of the R language definition manual:
# https://cran.r-project.org/doc/manuals/R-lang.html#Subset-assignment
get_assigned_var <- function(e) {
  v <- e[[2]]
  if (missing(v)) {
    stop(paste("missing assignment variable in", dsq(e)), call. = FALSE)
  } else if (typeof(v) %in% c("symbol", "character")) {
    as.character(v)
  } else {
    while (typeof(v) == "language") {
      if (length(v) < 2) {
        stop(paste("unfinished code:", dsq(e)), call. = FALSE)
      }
      v <- v[[2]]
      if (missing(v)) {
        stop(paste("missing variable in", dsq(e)), call. = FALSE)
      }
    }
    if (typeof(v) != "symbol") {
      stop(paste("not a symbol:", dsq(e)), call. = FALSE)
    }
    as.character(v)
  }
}

flatten_assignment <- function(e) {
  if (typeof(e) == "language") {
    c(evalseq(e[[2]]), apdef(e))
  } else {
    # Was list(NULL, NULL), but that seems unnecessary here. # nolint
    NULL
  }
}

evalseq <- function(e) {
  if (typeof(e) == "language") {
    v <- evalseq(e[[2]])
    e[[2]] <- codetools_tmp
    c(v, list(e))
  }
  else {
    list(e)
  }
}

apdef <- function(e) {
  v <- NULL
  while (typeof(e) == "language") {
    ef <- e
    ef[[1]] <- make_assignment_fn(e[[1]])
    if (typeof(ef[[2]]) == "language") {
      ef[[2]] <- codetools_tmp
    }
    ef$value <- codetools_tmpv
    v <- c(v, list(ef))
    e <- e[[2]]
  }
  v
}

make_assignment_fn <- function(fun) {
  if (typeof(fun) == "symbol") {
    as.name(paste0(as.character(fun), "<-"))
  } else {
    is_correctly_namespaced <- typeof(fun) == "language" &&
      typeof(fun[[1]]) == "symbol" &&
      as.character(fun[[1]]) %in% c("::", ":::") &&
      length(fun) == 3 &&
      typeof(fun[[3]]) == "symbol"
    if (is_correctly_namespaced) {
      fun[[3]] <- as.name(paste0(as.character(fun[[3]]), "<-"))
      fun
    }
    else {
      stop("bad function in complex assignments: ", dsq(fun), call. = FALSE)
    }
  }
}

dsq <- function(e) {
  sQuote(safe_deparse(e))
}
