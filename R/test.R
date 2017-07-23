# Use testopts() to run all the unit tests
# for different modes of parallel computing: different
# numbers of jobs and mclapply vs Makefiles.
# For a really thorough test session, 
# be sure to also toggle the environment (global vs custom)
# at the top of debug.R.

testopts = function(){list(
  jobs = 1, # needs to be 1 for mclapply on Windows
  parallelism = "parLapply") # use for shipped tests
 # parallelism = "mclapply")
 # parallelism = "Makefile")
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
  sapply(config$cache$list(namespace = "progress"),
    function(target)
      config$cache$get(key = target, namespace = "progress")) %>%
    Filter(f = function(x) x == "finished") %>% names %>%
    intersect(y = config$plan$target) %>% sort
}

nobuild = function(config){
  expect_true(length(justbuilt(config)) < 1)
}
