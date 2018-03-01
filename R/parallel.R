run_parallel_backend <- function(config){
  get(
    paste0("run_", config$parallelism),
    envir = getNamespace("drake")
  )(config)
}

parallel_filter <- function(x, f, jobs = 1, ...){
  index <- lightly_parallelize(X = x, FUN = f, jobs = jobs, ...)
  x[as.logical(index)]
}

lightly_parallelize <- function(X, FUN, jobs = 1, ...) {
  jobs <- safe_jobs_imports(jobs)
  if (is.atomic(X)){
    lightly_parallelize_atomic(X = X, FUN = FUN, jobs = jobs, ...)
  } else {
    mclapply(X = X, FUN = FUN, mc.cores = jobs, ...)
  }
}

lightly_parallelize_atomic <- function(X, FUN, jobs = 1, ...){
  jobs <- safe_jobs_imports(jobs)
  keys <- unique(X)
  index <- match(X, keys)
  values <- mclapply(X = keys, FUN = FUN, mc.cores = jobs, ...)
  values[index]
}

jobs_imports <- function(jobs){
  check_jobs(jobs)
  if (length(jobs) < 2){
    jobs
  } else {
    unname(jobs["imports"])
  }
}

jobs_targets <- function(jobs){
  check_jobs(jobs)
  if (length(jobs) < 2){
    jobs
  } else {
    unname(jobs["targets"])
  }
}

safe_jobs <- function(jobs){
  stopifnot(length(jobs) == 1)
  ifelse(on_windows(), 1, jobs)
}

safe_jobs_imports <- function(jobs){
  ifelse(on_windows(), 1, jobs_imports(jobs = jobs))
}

on_windows <- function(){
  this_os() == "windows"
}

this_os <- function(){
  Sys.info()["sysname"] %>%
    tolower %>%
    unname
}

parallelism_warnings <- function(config){
  warn_mclapply_windows(
    parallelism = config$parallelism,
    jobs = config$jobs,
    os = this_os()
  )
}

use_default_parallelism <- function(parallelism){
  parallelism <- match.arg(
    parallelism,
    choices = parallelism_choices(distributed_only = FALSE)
  )
  if (parallelism %in% parallelism_choices(distributed_only = TRUE)){
    parallelism <- default_parallelism()
  }
  parallelism
}
