run_parLapply = function(config){
  if(config$jobs < 2){
    run_lapply(config = config)
    return(invisible())
  }
  outfile = ifelse(config$verbose, "", "/dev/null")
  config$cluster = makePSOCKcluster(config$jobs, outfile = outfile)
  clusterExport(cl = config$cluster,
    varlist = c("config", ls("package:drake", all.names = TRUE)),
    envir = environment())
  if(identical(config$envir, globalenv()))
    clusterExport(cl = config$cluster,
      varlist = ls(globalenv(), all.names = TRUE), envir = globalenv())
  clusterCall(cl = config$cluster, fun = load_packages_parLapply)
  clusterCall(cl = config$cluster, fun = do_prework,
    config = config, verbosePackages = FALSE)
  run_parallel(config = config, worker = worker_parLapply)
  stopCluster(cl = config$cluster)
}

worker_parLapply = function(targets, hash_list, config){
  prune_envir_parLapply(targets = targets, config = config)
  values = parLapply(cl = config$cluster, X = targets, fun = build,
    hash_list = hash_list, config = config)
  assign_to_envir_parLapply(target = targets, value = values,
      config = config)
}

prune_envir_parLapply = function(targets = targets, config = config){
  prune_envir(targets = targets, config = config)
  if(identical(config$envir, globalenv()))
    clusterCall(cl = config$cluster, fun = prune_envir, targets = targets,
      config = config)
}

assign_to_envir_parLapply = function(target, value, config){
  assign_to_envir(target = target, value = value, config = config)
  if(identical(config$envir, globalenv()))
    clusterCall(cl = config$cluster, fun = assign_to_envir,
      target = target, value = value, config = config)
}

load_packages_parLapply = function(){
  # Not ideal, but necessary.
  packages = c(
    "base64url", "codetools", "crayon", "eply", "digest", "igraph",
    "magrittr", "parallel", "plyr", "R.utils", "storr", "stringi", 
    "stringr", "testthat", "tools", "utils")
  for(package in packages)
    suppressPackageStartupMessages(require(package, character.only = TRUE))
}
