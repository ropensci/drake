# Use testopts() to run all the unit tests
# for different modes of parallel computing: different
# numbers of jobs and mclapply vs Makefiles.
# For a really thorough test session, 
# be sure to also toggle the environment (global vs custom)
# at the top of debug.R.

testopts = function(){list(
  jobs = 2, # needs to be 1 for mclapply on Windows
  parallelism = "parLapply") # use for shipped tests
 # parallelism = "mclapply")
 # parallelism = "Makefile")
} # drake has to already be installed for the Makefile stuff

testrun = function(config){
  opt = test_opt()
  make(plan = config$plan, targets = config$targets,
    envir = config$envir, verbose = FALSE,
    parallelism = opt$parallelism, jobs = opt$jobs,
    packages = config$packages, prework = config$prework,
    prepend = config$prepend, command = config$command)
}

testrun_automatic_packages = function(config){
  opts = test_opt()
  make(plan = config$plan, targets = config$targets,
      envir = config$envir, verbose = FALSE,
      parallelism = opt$parallelism, jobs = opt$jobs,
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

set_test_opt <- function(opt){
  opt = match.arg(opt, choices = names(test_opts))
  options(drake_test_opt = opt)
}

test_opt <- function(){
  opt <- getOption("drake_test_opt")
  if(!length(opt)) opt <- names(test_opts)[1]
  test_opts[[opt]]
}

test_opts <-
  list(
    parent_parL_2 = list(
      envir = "new.env(parent = globalenv())",
      parallelism = "parLapply",
      jobs = 2,
      cran = TRUE
      ), # For CRAN, Travis, and Appveyor, only use this configuration.
    parent_parL_1 = list(
      envir = "new.env(parent = globalenv())",
      parallelism = "parLapply",
      jobs = 1
      ), # Uses lapply() instead of parLapply() when jobs = 1.
    parent_mcl_1 = list(
      envir = "new.env(parent = globalenv())",
      parallelism = "mclapply",
      jobs = 1
      ), #
    parent_mcl_8 = list(
      envir = "new.env(parent = globalenv())",
      parallelism = "mclapply",
      jobs = 8,
      skip_os = c("windows")
      ), # Skip on Windows.
    parent_Make_1 = list(
      envir = "new.env(parent = globalenv())",
      parallelism = "Makefile",
      jobs = 1
      ), #
    # Makefiles are different, so I want to test with a low and a
    # high jobs value.
    parent_Make_2 = list(
      envir = "new.env(parent = globalenv())",
      parallelism = "Makefile",
      jobs = 2
      ),
    parent_Make_16 = list(
      envir = "new.env(parent = globalenv())",
      parallelism = "Makefile",
      jobs = 16
      ), #
    # For the global environment, I do not think all scenarios need to be
    # repeated.
    global_parL_1 = list(
      envir = "globalenv()",
      parallelism = "parLapply",
      jobs = 1
      ),
    global_parL_2 = list(
      envir = "globalenv()",
      parallelism = "parLapply",
      jobs = 2
      ), #
    new_mcl_2 = list(
      envir = "new.env()",
      parallelism = "mclapply",
      jobs = 1
      ), #
    global_mcl_8 = list(
      envir = "globalenv()",
      parallelism = "mclapply",
      jobs = 8,
      skip_os = c("windows")
      ), # Skip on Windows.
    global_Make_16 = list(
      envir = "globalenv()",
      parallelism = "Makefile",
      jobs = 16
      ) #
  )
