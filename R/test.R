drake_context <- function(x){
  assert_pkg("testthat")
  ctx <- paste0(get_testing_scenario_name(), ": ", x)
  testthat::context(ctx)
}

testrun <- function(config) {
  set_test_backend()
  invisible(
    make(plan = config$plan, targets = config$targets, envir = config$envir,
      verbose = config$verbose, parallelism = config$parallelism,
      jobs = config$jobs,
      packages = config$packages, prework = config$prework,
      prepend = config$prepend, command = config$command,
      cache = config$cache, lazy_load = config$lazy_load,
      session_info = config$session_info,
      fetch_cache = config$fetch_cache,
      caching = config$caching
    )
  )
}

justbuilt <- function(config) {
  recorded <- config$cache$list(namespace = "progress")
  all <- lightly_parallelize(
    X = recorded,
    FUN = function(target){
      config$cache$get(
        key = target, namespace = "progress", use_cache = FALSE)
    },
    jobs = config$jobs
  )
  names(all) <- recorded
  unlist(all) %>%
    Filter(f = function(x) x == "finished") %>%
    names %>%
    intersect(y = config$plan$target) %>%
    sort
}

nobuild <- function(config) {
  assert_pkg("testthat")
  testthat::expect_true(length(justbuilt(config)) < 1)
}

#' @title Run a unit test in a way that quarantines
#'   the side effects from your workspace and file system.
#' @description Typical users of drake should not need this function.
#' It is exported so it can be used to quarantine the side effects
#' of the examples in the help files.
#' @export
#' @keywords internal
#' @return nothing
#' @param desc character, description of the test
#' @param ... code to test
#' @examples
#' \dontrun{
#' test_with_dir(
#'   "Write a file to a temporary folder",
#'   writeLines("hello", "world.txt")
#' )
#' file.exists("world.txt") # FALSE
#' }
test_with_dir <- function(desc, ...){
  assert_pkg("testthat")
  while (file.exists(new <- tempfile())){
    # Should not reach this part of the loop.
    Sys.sleep(0.01) # nocov
  }
  dir.create(new)
  withr::local_dir(new)
  withr::local_options(new = list(clustermq.scheduler = "multicore"))
  set_test_backend()
  testthat::test_that(desc = desc, ...)
  invisible()
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
  assert_pkg("rprojroot")
  root <- rprojroot::find_root(criterion = "DESCRIPTION", path = path)
  file.path(root, "tests", "testthat")
}

with_all_options <- function(code) {
  old <- options()
  on.exit(restore_options(old))
  force(code)
}

write_v4.3.0_project <- function(){ # nolint
  zip <- system.file(
    file.path("testing", "built_mtcars_example_v4.3.0.zip"),
    package = "drake",
    mustWork = TRUE
  )
  unzip(zip, exdir = ".", setTimes = TRUE)
}
