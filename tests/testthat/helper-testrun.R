testrun <- function(config) {
  set_test_backend()
  invisible(
    make(
      plan = config$plan,
      targets = config$targets,
      envir = config$envir,
      verbose = config$verbose,
      parallelism = config$parallelism,
      jobs = config$jobs,
      packages = config$packages,
      prework = config$prework,
      prepend = config$prepend,
      command = config$command,
      cache = config$cache,
      lazy_load = config$lazy_load,
      session_info = config$session_info,
      fetch_cache = config$fetch_cache,
      caching = config$caching,
      lock_envir = !any(grepl("staged", config$parallelism))
    )
  )
}

testconfig <- function(config) {
  out <- drake_config(
    plan = config$plan,
    targets = config$targets,
    envir = config$envir,
    verbose = config$verbose,
    parallelism = config$parallelism,
    jobs = config$jobs,
    packages = config$packages,
    prework = config$prework,
    prepend = config$prepend,
    command = config$command,
    cache = config$cache,
    lazy_load = config$lazy_load,
    session_info = config$session_info,
    fetch_cache = config$fetch_cache,
    caching = config$caching,
    lock_envir = !any(grepl("staged", config$parallelism))
  )
  out$plan <- config$plan
  out$targets <- config$targets
  out
}
