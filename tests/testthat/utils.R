testopts = function(){list(
  jobs = 1,
  parallelism = "single-session")
  #parallelism = "distributed")
}

testrun = function(args){
  o = testopts()
  run(plan = args$plan, targets = args$targets,
    envir = args$envir, verbose = FALSE,
    parallelism = o$parallelism, jobs = o$jobs, 
    packages = args$packages, prework = args$prework,
    prepend = args$prepend, command = args$command,
    arg = args$args)
}

testrun_packages = function(args){
  o = testopts()
  run(plan = args$plan, targets = args$targets,
      envir = args$envir, verbose = FALSE,
      parallelism = o$parallelism, jobs = o$jobs, 
      prework = args$prework,
      prepend = args$prepend, command = args$command,
      arg = args$args)
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
