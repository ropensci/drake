parallel_filter <- function(x, f, jobs = 1, ...) {
  index <- lightly_parallelize(X = x, FUN = f, jobs = jobs, ...)
  index <- unlist(index)
  x[as.logical(index)]
}

lightly_parallelize <- function(X, FUN, jobs = 1, ...) {
  jobs <- safe_jobs(jobs)
  if (is.atomic(X)) {
    lightly_parallelize_atomic(X = X, FUN = FUN, jobs = jobs, ...)
  } else {
    weak_mclapply(X = X, FUN = FUN, mc.cores = jobs, ...)
  }
}

lightly_parallelize_atomic <- function(X, FUN, jobs = 1, ...) {
  jobs <- safe_jobs(jobs)
  keys <- unique(X)
  index <- match(X, keys)
  values <- weak_mclapply(X = keys, FUN = FUN, mc.cores = jobs, ...)
  values[index]
}

# Avoid SIGCHLD handler when mc.cores is 1.
# Could help avoid zeromq interrupted system call errors.
weak_mclapply <- function(X, FUN, mc.cores, ...) {
  if (mc.cores > 1) {
    parallel::mclapply(X = X, FUN = FUN, mc.cores = mc.cores, ...)
  } else {
    lapply(X = X, FUN = FUN, ...)
  }
}

safe_jobs <- function(jobs) {
  stopifnot(length(jobs) == 1)
  ifelse(on_windows(), 1, jobs)
}

on_windows <- function() {
  this_os() == "windows"
}

this_os <- function() {
  unname(tolower(Sys.info()["sysname"]))
}
