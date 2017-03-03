testopts = function(){list(
  jobs = 1, # needs to be 1 for shipped unit tests
  parallelism = "single-session")
 # parallelism = "distributed") # can't use for shipped unit tests
}

testrun = function(args){
  opts = testopts()
  run(plan = args$plan, targets = args$targets,
    envir = args$envir, verbose = FALSE,
    parallelism = opts$parallelism, jobs = opts$jobs, 
    packages = args$packages, prework = args$prework,
    prepend = args$prepend, command = args$command)
}

testrun_automatic_packages = function(args){
  opts = testopts()
  run(plan = args$plan, targets = args$targets,
      envir = args$envir, verbose = FALSE,
      parallelism = opts$parallelism, jobs = opts$jobs, 
      prework = args$prework,
      prepend = args$prepend, command = args$command)
}

justbuilt = function(args){
  setdiff(status()$target, imported())
}

nobuild = function(args){
  built = status()$target
  targets = args$plan$target
  both = intersect(built, targets)
  expect_equal(both, character(0))
}
