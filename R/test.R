justbuilt <- function(config) {
  sapply(config$cache$list(namespace = "progress"),
    function(target)
      config$cache$get(key = target, namespace = "progress")) %>%
      Filter(f = function(x) x == "finished") %>%
      names %>% intersect(y = config$plan$target) %>% sort
}

nobuild <- function(config) {
  expect_true(length(justbuilt(config)) < 1)
}

show_config_opts <- function(config) {
  capture.output(print(config$envir)) %>% cat
  cat("_", config$parallelism, sep = "")
  cat("_", config$jobs, "_", sep = "")
}

test_with_dir <- function(desc, code){
  root <- "workspaces"
  if (!file.exists(root)){
    dir.create(root)
  }
  relative_dir <- base32_encode(desc) %>%
    paste(as.character(rnorm(1))) %>%
    digest(algo = hash_algorithm)
  dir <- file.path(root, relative_dir)
  dir_empty(dir)
  dir <- normalizePath(dir)
  with_dir(dir, test_that(desc = desc, code = code))
  unlink(dir, recursive = TRUE)
}

testrun <- function(config) {
  invisible(
    make(plan = config$plan, targets = config$targets, envir = config$envir,
      verbose = FALSE, parallelism = config$parallelism, jobs = config$jobs,
      packages = config$packages, prework = config$prework,
      prepend = config$prepend, command = config$command,
      return_config = TRUE))
}

set_test_opt <- function(opt) {
  opt <- match.arg(opt, choices = names(test_opts))
  options(drake_test_opt = opt)
}

test_opt <- function() {
  opt <- getOption("drake_test_opt")
  if (!length(opt))
    opt <- names(test_opts)[1]
  test_opts[[opt]]
}

test_opts <- list(
  local_parL_2 = list(
    envir = "new.env(parent = globalenv())",
    parallelism = "parLapply",
    jobs = 2,
    cran = TRUE
  ),
  local_parL_1 = list(
    envir = "new.env(parent = globalenv())",
    parallelism = "parLapply",
    jobs = 1
  ),
  local_mcl_1 = list(
    envir = "new.env(parent = globalenv())",
    parallelism = "mclapply",
    jobs = 1
  ),
  local_mcl_8 = list(
    envir = "new.env(parent = globalenv())",
    parallelism = "mclapply",
    jobs = 8,
    skip_os = "windows"
  ),
  local_Make_1 = list(
    envir = "new.env(parent = globalenv())",
    parallelism = "Makefile",
    jobs = 1
  ),
  local_Make_2 = list(
    envir = "new.env(parent = globalenv())",
    parallelism = "Makefile",
    jobs = 2
  ),
  local_Make_16 = list(
    envir = "new.env(parent = globalenv())",
    parallelism = "Makefile",
    jobs = 16
  ),
  global_parL_1 = list(
    envir = "globalenv()",
    parallelism = "parLapply",
    jobs = 1
  ),
  global_parL_2 = list(
    envir = "globalenv()",
    parallelism = "parLapply",
    jobs = 2
  ),
  global_mcl_1 = list(
    envir = "globalenv()",
    parallelism = "mclapply",
    jobs = 1
  ),
  global_mcl_8 = list(
    envir = "globalenv()",
    parallelism = "mclapply",
    jobs = 8,
    skip_os = "windows"
  ),
  global_Make_16 = list(
    envir = "globalenv()",
    parallelism = "Makefile",
    jobs = 16
  )
)
