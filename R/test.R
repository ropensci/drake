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

test_configs <- function(){
  list(
    list(
      label = "parent_parL_1",
      envir = "new.env(parent = globalenv())",
      parallelism = "parLapply",
      jobs = 1
      ), # Uses lapply() instead of parLapply() when jobs = 1.
    list(
      label = "parent_parL_2",
      envir = "new.env(parent = globalenv())",
      parallelism = "parLapply",
      jobs = 2,
      cran = TRUE
      ), # For CRAN, Travis, and Appveyor, only use this configuration.
    list(
      label = "parent_mcl_1",
      envir = "new.env(parent = globalenv())",
      parallelism = "mclapply",
      jobs = 1
      ), #
    list(
      label = "parent_mcl_8",
      envir = "new.env(parent = globalenv())",
      parallelism = "mclapply",
      jobs = 8,
      skip_os = c("windows")
      ), # Skip on Windows.
    list(
      label = "parent_Make_1",
      envir = "new.env(parent = globalenv())",
      parallelism = "Makefile",
      jobs = 1
      ), #
    # Makefiles are different, so I want to test with a low and a
    # high jobs value.
    list(
      label = "parent_Make_2",
      envir = "new.env(parent = globalenv())",
      parallelism = "Makefile",
      jobs = 2
      ),
    list(
      label = "parent_Make_16",
      envir = "new.env(parent = globalenv())",
      parallelism = "Makefile",
      jobs = 16
      ), #
    # For the global environment, I do not think all scenarios need to be
    # repeated.
    list(
      label = "global_parL_1",
      envir = "globalenv()",
      parallelism = "parLapply",
      jobs = 1
      ),
    list(
      label = "global_parL_2",
      envir = "globalenv()",
      parallelism = "parLapply",
      jobs = 2
      ), #
    list(
      label = "new_mcl_2",
      envir = "new.env()",
      parallelism = "mclapply",
      jobs = 1
      ), #
    list(
      label = "global_mcl_8",
      envir = "globalenv()",
      parallelism = "mclapply",
      jobs = 8,
      skip_os = c("windows")
      ), # Skip on Windows.
    list(
      label = "global_Make_16",
      envir = "globalenv()",
      parallelism = "Makefile",
      jobs = 16
      ) #
  )
}

skip_tests <- function(config){
  if (!(length(config[["CRAN"]]) && config[["CRAN"]])){
    testthat::skip_on_cran()
    testthat::skip_on_travis()
    testthat::skip_on_appveyor()
  }
  if (length(config[["skip_os"]])){
    testthat::skip_on_os(config[["skip_os"]])
  }
}
