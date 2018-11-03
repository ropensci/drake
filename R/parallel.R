run_parallel_backend <- function(config){
  get(
    paste0("run_", config$parallelism),
    envir = getNamespace("drake")
  )(config)
}

parallel_filter <- function(x, f, jobs = 1, ...){
  index <- lightly_parallelize(X = x, FUN = f, jobs = jobs, ...)
  index <- unlist(index)
  x[as.logical(index)]
}

lightly_parallelize <- function(X, FUN, jobs = 1, ...) {
  jobs <- safe_jobs_imports(jobs)
  if (is.atomic(X)){
    lightly_parallelize_atomic(X = X, FUN = FUN, jobs = jobs, ...)
  } else {
    safe_mclapply(X = X, FUN = FUN, mc.cores = jobs, ...)
  }
}

lightly_parallelize_atomic <- function(X, FUN, jobs = 1, ...){
  jobs <- safe_jobs_imports(jobs)
  keys <- unique(X)
  index <- match(X, keys)
  values <- safe_mclapply(X = keys, FUN = FUN, mc.cores = jobs, ...)
  values[index]
}

# Avoid SIGCHLD handler when mc.cores is 1.
# Could help avoid zeromq interrupted system call errors.
safe_mclapply <- function(X, FUN, mc.cores, ...){
  if (mc.cores > 1){
    parallel::mclapply(X = X, FUN = FUN, mc.cores = mc.cores, ...)
  } else {
    lapply(X = X, FUN = FUN, ...)
  }
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
  unname(tolower(Sys.info()["sysname"]))
}

parallelism_warnings <- function(config){
  warn_mclapply_windows(
    parallelism = config$parallelism,
    jobs = config$jobs,
    os = this_os()
  )
}
