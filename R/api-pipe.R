# Functionality taken from magrittr:::split_chain()
# under the MIT license.
# https://github.com/tidyverse/magrittr/blob/4104d6b593e409859befd0076ddc1abd0417d793/R/split_chain.R#L10-L45 # nolint
split_dp_chain <- function (expr, envir) {
  links <- list()
  i <- 1L
  while (is.call(expr) && identical(expr[[1L]], quote(`%dp%`))) {
    link <- expr[[3L]]
    if (is_parenthesized(link)) {
      link <- eval(link, envir = envir, enclos = envir)
    }
    if (not_call(link)) {
      link <- as.call(list(link, quote(.)))
    } else if (no_dot_arg(link)) {
      link <- as.call(c(link[[1]], quote(.), as.list(link[-1])))
    }
    if (is.call(link) && identical(link[[1L]], quote(`function`))) {
      stop("Anonymous functions must be parenthesized", call. = FALSE)
    }
    links[[i]] <- link
    expr <- expr[[2L]]
    i <- i + 1L
  }
  links[[i]] <- expr
  rev(links)
}

resolve_drake_pipe_row <- function(plan, row, envir) {
  plan <- plan[row,, drop = FALSE] # nolint
  commands <- split_dp_chain(plan$command[[1]], envir)
  if (length(commands) < 2L) {
    return(plan)
  }
  targets <- rev(make.unique(rep(plan$target[1], length(commands))))
  for (i in seq(2, length(targets))) {
    sub <- list(. = as.name(targets[i - 1]))
    commands[[i]] <- eval(
      call("substitute", commands[[i]], sub),
      envir = baseenv()
    )
  }
  out <- lapply(seq_along(targets), function(i) {
    subplan <- plan
    subplan$target <- targets[i]
    subplan$command[[1]] <- commands[[i]]
    subplan
  })
  do.call(rbind, out)
}

resolve_drake_pipe <- function(plan, envir) {
  if (!nrow(plan)) {
    return(plan)
  }
  out <- lapply(
    seq_len(nrow(plan)),
    resolve_drake_pipe_row,
    plan = plan,
    envir = envir
  )
  drake_bind_rows(out)
}

is_parenthesized <- function(expr) {
  is.call(expr) && identical(expr[[1]], quote(`(`))
}

not_call <- function(expr) {
  is.symbol(expr) || is.function(expr)
}

no_dot_arg <- function(expr) {
  !any(vapply(expr[-1], identical, logical(1), quote(.)))
}
