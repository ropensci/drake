testrun <- function(config) {
  invisible(
    make(plan = config$plan, targets = config$targets, envir = config$envir,
      verbose = FALSE, parallelism = config$parallelism, jobs = config$jobs,
      packages = config$packages, prework = config$prework,
      prepend = config$prepend, command = config$command,
      return_config = TRUE, cache = config$cache
    )
  )
}

justbuilt <- function(config) {
  sapply(config$cache$list(namespace = "progress"),
    function(target)
      config$cache$get(key = target, namespace = "progress")) %>%
      Filter(f = function(x) x == "finished") %>%
      names %>%
      intersect(y = config$plan$target) %>%
      sort
}

nobuild <- function(config) {
  expect_true(length(justbuilt(config)) < 1)
}

test_with_dir <- function(desc, ...){
  root <- tempdir()
  stopifnot(file.exists(root))
  relative_dir <- digest(desc, algo = default_short_hash_algo())
  dir <- file.path(root, relative_dir)
  dir_empty(dir)
  with_dir(dir, test_that(desc = desc, ...))
}

unit_test_files <- function(path = getwd()){
  root <- find_root(criterion = "DESCRIPTION", path = path)
  file.path(root, "tests", "testthat")
}

this_os <- function(){
  Sys.info()["sysname"] %>%
    tolower %>%
    unname
}

should_skip <- function(scenario_name, os = this_os()){
  os %in% testing_scenarios[[scenario_name]]$skip_os
}
