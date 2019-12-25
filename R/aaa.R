config_util_body <- function(impl_fun) {
  env <- list(
    impl_fun = substitute(impl_fun),
    config_pos = substitute(config_pos)
  )
  substitute({
    # nocov start
    config <- config %|||% first_config(unnamed(list(...)))
    if (inherits(config, "drake_config")) {
      # 2019-12-21 # nolint
      deprecate_arg(
        config,
        "config",
        "... to supply make() arguments such as the plan"
      )
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
  }, env = env)
}

first_config <- function(args) {
  for (arg in args) {
    if (inherits(arg, "drake_config")) {
      return(arg)
    }
  }
  NULL
}
