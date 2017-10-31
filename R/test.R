drake_context <- function(x){
  ctx <- paste0(get_testing_scenario_name(), ": ", x)
  context(ctx)
}

testrun <- function(config) {
  set_test_backend()
  invisible(
    make(plan = config$plan, targets = config$targets, envir = config$envir,
      verbose = config$verbose, parallelism = config$parallelism,
      jobs = config$jobs,
      packages = config$packages, prework = config$prework,
      prepend = config$prepend, command = config$command,
      cache = config$cache
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
  relative_dir <- digest::digest(
    list(desc, sessionInfo()$platform, rnorm(1)),
    algo = default_short_hash_algo()
  )
  dir <- file.path(root, relative_dir)
  dir_empty(dir)
  with_dir(
    new = dir,
    code = {
      set_test_backend()
      tmp <- capture.output(
        test_that(desc = desc, ...)
      )
    }
  )
}

restore_options <- function(old){
  current <- options()
  remove_these <- setdiff(names(current), names(old))
  removal_list <- as.list(old[remove_these])
  names(removal_list) <- remove_these
  do.call(options, removal_list)
  options(old)
}

set_test_backend <- function(){
  eval(parse(text = get_testing_scenario()$backend))
}

unit_test_files <- function(path = getwd()){
  root <- find_root(criterion = "DESCRIPTION", path = path)
  file.path(root, "tests", "testthat")
}

with_all_options <- function(code) {
  old <- options()
  on.exit(restore_options(old))
  force(code)
}
