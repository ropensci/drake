testopts = function(){list(
  jobs = 1, # needs to be 1 for shipped unit tests
  parallelism = "mclapply")
 # parallelism = "Makefile") # can't use for shipped unit tests
} # drake has to already be installed for the Makefile stuff

testrun = function(config){
  opts = testopts()
  run(plan = config$plan, targets = config$targets,
    envir = config$envir, verbose = FALSE,
    parallelism = opts$parallelism, jobs = opts$jobs, 
    packages = config$packages, prework = config$prework,
    prepend = config$prepend, command = config$command)
}

testrun_automatic_packages = function(config){
  opts = testopts()
  run(plan = config$plan, targets = config$targets,
      envir = config$envir, verbose = FALSE,
      parallelism = opts$parallelism, jobs = opts$jobs, 
      prework = config$prework,
      prepend = config$prepend, command = config$command)
}

justbuilt = function(config){
  setdiff(status()$target, imported())
}

nobuild = function(config){
  built = status()$target
  targets = config$plan$target
  both = intersect(built, targets)
  expect_equal(both, character(0))
}
