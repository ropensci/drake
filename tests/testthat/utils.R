testrun = function(args){
  jobs = 1
  parallelism = "single-session"
                #"distributed"
  run(plan = args$plan, targets = args$targets,
    envir = args$envir, jobs = args$jobs, 
    parallelism = parallelism, verbose = FALSE)
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
