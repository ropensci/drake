run_parLapply <- function(config) { # nolint
  assert_pkg("parallel")
  eval(parse(text = "require(drake)"))
  if (config$jobs < 2 && !length(config$debug)) {
    return(run_loop(config = config))
  }
  assert_pkg("txtq")
  console_parLapply(config) # nolint
  config$cluster <- parallel::makePSOCKcluster(config$jobs + 1)
  on.exit(parallel::stopCluster(cl = config$cluster))
  parallel::clusterExport(
    cl = config$cluster,
    varlist = "config",
    envir = environment()
  )
  if (identical(config$envir, globalenv())) {
    # We should not use globalenv() in regular unit tests.
    # drake has other tests for that.
    # nocov start
    parallel::clusterExport(
      cl = config$cluster,
      varlist = ls(globalenv(),
      all.names = TRUE), envir = globalenv()
    )
    # nocov end
  }
  parallel::clusterCall(cl = config$cluster, fun = function() {
    eval(parse(text = "suppressPackageStartupMessages(require(drake))"))
  })
  parallel::clusterCall(
    cl = config$cluster,
    fun = do_prework,
    config = config,
    verbose_packages = FALSE
  )
  mc_init_worker_cache(config)
  parallel::parLapply(
    cl = config$cluster,
    X = mc_worker_id(c(0, seq_len(config$jobs))),
    fun = mc_process,
    config = config
  )
  invisible()
}
