run_parallel_backend <- function(config){
  config$workers <- as.character(seq_len(config$jobs))
  config$cache$set(
    key = "workers",
    value = config$workers,
    namespace = "config"
  )
  get(
    paste0("run_", config$parallelism),
    envir = getNamespace("drake")
  )(config)
}

parallel_filter <- function(x, f, jobs = 1, ...){
  index <- lightly_parallelize(X = x, FUN = f, jobs = jobs, ...) %>%
    unlist
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

# x is parallelism or jobs
imports_setting <- function(x){
  if (length(x) < 2){
    x
  } else {
    unname(x["imports"])
  }
}

# x is parallelism or jobs
targets_setting <- function(x){
  if (length(x) < 2){
    x
  } else {
    unname(x["targets"])
  }
}

safe_jobs <- function(jobs){
  stopifnot(length(jobs) == 1)
  ifelse(on_windows(), 1, jobs)
}

safe_jobs_imports <- function(jobs){
  ifelse(on_windows(), 1, imports_setting(jobs))
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
