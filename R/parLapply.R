run_parLapply = function(config){
  outfile = ifelse(config$verbose, "", "/dev/null")
  config$cluster = makePSOCKcluster(config$jobs, outfile = outfile)
  clusterExport(cl = config$cluster, varlist = "config",
    envir = environment())
  clusterCall(cl = config$cluster, fun = do_prework, 
    config = config, verbosePackages = FALSE)
  run_parallel(config = config, worker = worker_parLapply)
  stopCluster(cl = config$cluster)
}

worker_parLapply = function(targets, hash_list, config){
  parLapply(cl = config$cluster, X = targets, fun = build, 
    hash_list = hash_list, config = config)
}
