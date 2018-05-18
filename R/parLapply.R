run_parLapply <- function(config) { # nolint
  eval(parse(text = "require(drake)"))
  if (config$jobs < 2 && !length(config$debug)) {
    return(run_loop(config = config))
  }
  config$workers <- as.character(seq_len(config$jobs))
  console_parLapply(config) # nolint
  config$cluster <- makePSOCKcluster(config$jobs + 1)
  on.exit(stopCluster(cl = config$cluster))
  clusterExport(cl = config$cluster, varlist = "config",
    envir = environment())
  if (identical(config$envir, globalenv()))
    clusterExport(cl = config$cluster, varlist = ls(globalenv(),
      all.names = TRUE), envir = globalenv())
  clusterCall(cl = config$cluster, fun = function(){
    eval(parse(text = "require(drake)"))
  })
  clusterCall(cl = config$cluster, fun = do_prework, config = config,
    verbose_packages = FALSE)
  mc_init_worker_cache(config)
  parLapply(
    cl = config$cluster,
    X = c("0", config$workers),
    fun = mc_process,
    config = config
  )
  invisible()
}
