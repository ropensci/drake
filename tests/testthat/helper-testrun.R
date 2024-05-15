testrun <- function(config) {
  set_test_backend()
  invisible(
    make(
      plan = config$plan,
      targets = config$targets,
      envir = config$envir,
      verbose = config$logger$verbose,
      parallelism = config$settings$parallelism,
      jobs = config$settings$jobs,
      packages = config$packages,
      prework = config$prework,
      prepend = config$prepend,
      command = config$command,
      cache = config$cache,
      lazy_load = config$settings$lazy_load,
      session_info = config$settings$session_info,
      fetch_cache = config$fetch_cache,
      caching = config$caching
    )
  )
}

testconfig <- function(config) {
  out <- drake_config(
    plan = config$plan,
    targets = config$targets,
    envir = config$envir,
    verbose = config$logger$verbose,
    parallelism = config$settings$parallelism,
    jobs = config$settings$jobs,
    packages = config$packages,
    prework = config$prework,
    prepend = config$prepend,
    command = config$command,
    cache = config$cache,
    lazy_load = config$settings$lazy_load,
    session_info = config$settings$session_info,
    fetch_cache = config$fetch_cache,
    caching = config$caching
  )
  out$plan <- config$plan
  out$targets <- config$targets
  out
}
