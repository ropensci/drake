config_util_body <- function(impl_fun) {
  substitute({
    # nocov start
    config <- config %|||% unnamed(list(...))[[1]]
    if (inherits(config, "drake_config")) {
      # 2019-12-21 # nolint
      deprecate_arg(config, "config", "... to supply the plan etc.")
      call <- match.call(definition = impl_fun, expand.dots = TRUE)
      call[[1]] <- quote(impl_fun)
      return(eval(call))
    }
    for (arg in list(...)) {
      force(arg) # could be a custom envir
    }
    config <- drake_config2(...)
    call <- match.call(expand.dots = FALSE)
    call[[1]] <- quote(impl_fun)
    call$config <- NULL
    call$... <- quote(config)
    names(call)[names(call) == "..."] <- "config"
    eval(call)
    # nocov end
  }, env = list(impl_fun = substitute(impl_fun)))
}
