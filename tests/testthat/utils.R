testopts = function(){list(
  jobs = 2, # needs to be 1 for shipped unit tests
 # parallelism = "mclapply")
  parallelism = "Makefile") # can't use for shipped unit tests
} # drake has to already be installed for the Makefile stuff

testrun = function(config){
  opts = testopts()
  make(plan = config$plan, targets = config$targets,
    envir = config$envir, verbose = FALSE,
    parallelism = opts$parallelism, jobs = opts$jobs, 
    packages = config$packages, prework = config$prework,
    prepend = config$prepend, command = config$command)
}

testrun_automatic_packages = function(config){
  opts = testopts()
  make(plan = config$plan, targets = config$targets,
      envir = config$envir, verbose = FALSE,
      parallelism = opts$parallelism, jobs = opts$jobs, 
      prework = config$prework,
      prepend = config$prepend, command = config$command)
}

justbuilt = function(config){
  sapply(config$cache$list(namespace = "status"), 
    function(target) config$cache$get(key = target, namespace = "status")) %>%
    Filter(f = function(x) x == "finished") %>% names %>%
    intersect(y = config$plan$target)
}

nobuild = function(config){
  expect_true(length(justbuilt(config)) < 1)
}
