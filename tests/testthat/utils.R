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
  status(imported_files_only = TRUE) %>%
    Filter(f = function(x) x == "finished") %>% names %>%
    setdiff(y = imported(files_only = FALSE))
}

nobuild = function(config){
  if(!file.exists(cachepath)) return(invisible())
  builds = status(imported_files_only = TRUE) %>% names %>%
    intersect(y = built())
  expect_true(length(builds) < 1)
}
