drake_context("parallel")

test_with_dir("safe_jobs()", {
  expect_error(safe_jobs(1:3))
  expect_true(is.numeric(safe_jobs(1)))
})

test_with_dir("check_jobs()", {
  expect_error(check_jobs(NULL), regexp = "length")
  expect_error(check_jobs(-1), regexp = "jobs > 0")
  expect_error(check_jobs(c(-1, 1)), regexp = "jobs > 0")
  expect_error(check_jobs(c(a = 1, targets = 5)), regexp = "targets")
  expect_error(check_jobs(1:2))
  expect_silent(check_jobs(1))
  expect_silent(check_jobs(c(targets = 5, imports = 6)))
  expect_silent(check_jobs(c(imports = 5, targets = 6)))
})

test_with_dir("jobs_imports()", {
  expect_equal(jobs_imports(8), 8)
  expect_error(jobs_imports(c(8, 12)))
  expect_error(jobs_imports(c(8, 1)))
  expect_equal(jobs_imports(c(targets = 8, imports = 12)), 12)
  expect_equal(jobs_imports(c(imports = 8, targets = 12)), 8)
})

test_with_dir("targets_setting()", {
  expect_equal(targets_setting(8), 8)
  expect_error(targets_setting(c(8, 12)))
  expect_error(targets_setting(c(8, 1)))
  expect_equal(targets_setting(c(targets = 8, imports = 12)), 8)
  expect_equal(targets_setting(c(imports = 8, targets = 12)), 12)
})

test_with_dir("parallelism not found for testrun()", {
  config <- list(parallelism = "not found", verbose = FALSE)
  suppressWarnings(expect_error(testrun(config)))
})

test_with_dir("parallelism_choices", {
  expect_true(
    length(parallelism_choices(distributed_only = TRUE)) <
    length(parallelism_choices(distributed_only = FALSE))
  )
})

test_with_dir("parallelism warnings", {
  config <- dbug()
  suppressWarnings(parallelism_warnings(config))
  expect_silent(
    warn_mclapply_windows(parallelism = "mclapply", jobs = 1)
  )
  expect_warning(
    warn_mclapply_windows(parallelism = "mclapply", jobs = 2, os = "windows")
  )
})

test_with_dir("shell_file() writes correctly", {
  expect_false(file.exists("shell.sh"))
  shell_file()
  expect_true(file.exists("shell.sh"))
  unlink("shell.sh", force = TRUE)

  d <- "exdir"
  dir.create(d)
  p <- file.path(d, "script.txt")
  expect_false(file.exists(p))
  shell_file(p)
  expect_true(file.exists(p))
  unlink(d, recursive = TRUE, force = TRUE)

  expect_silent(shell_file(overwrite = TRUE))
  expect_silent(shell_file(overwrite = FALSE))
  expect_warning(shell_file(overwrite = TRUE))
})

test_with_dir("mclapply and lapply", {
  config <- dbug()
  config$parallelism <- "parLapply"
  config$jobs <- 1
  config$debug <- TRUE
  suppressWarnings(out <- make(config = config))
  expect_true(length(justbuilt(out)) > 0)
  expect_true(is.numeric(readd(final)))
  suppressWarnings(out <- make(config = config))
  expect_true(is.numeric(readd(final)))
  expect_equal(justbuilt(out), character(0))
  skip_on_os("windows")
  config$parallelism <- "mclapply"
  clean()
  suppressWarnings(out <- make(config = config))
  expect_true(length(justbuilt(out)) > 0)
  expect_true(is.numeric(readd(final)))
  suppressWarnings(out <- make(config = config))
  expect_true(is.numeric(readd(final)))
  expect_equal(justbuilt(out), character(0))
})

test_with_dir("staged mclapply and lapply", {
  config <- dbug()
  env <- config$envir
  config$parallelism <- "parLapply_staged"
  config$jobs <- 1
  out <- make(config = config)
  clean()
  config$jobs <- 2
  config$debug <- TRUE
  suppressWarnings(out <- make(config = config))
  expect_true(length(justbuilt(out)) > 0)
  expect_true(is.numeric(readd(final)))
  suppressWarnings(out <- make(config = config))
  expect_true(is.numeric(readd(final)))
  expect_equal(justbuilt(out), character(0))
  expect_true(is.numeric(readd(final)))
  expect_equal(justbuilt(out), character(0))
  skip_on_os("windows")
  config$parallelism <- "mclapply_staged"
  clean()
  config$envir <- env
  suppressWarnings(out <- make(config = config))
  expect_true(length(justbuilt(out)) > 0)
  expect_true(is.numeric(readd(final)))
  suppressWarnings(out <- make(config = config))
  expect_true(is.numeric(readd(final)))
  expect_equal(justbuilt(out), character(0))
})

test_with_dir("lightly_parallelize_atomic() is correct", {
  withr::with_seed(seed = 2017, code = {
    x <- sample(LETTERS[1:3], size = 1e3, replace = TRUE)
    append <- function(x){
      paste0(x, "_text")
    }
    out0 <- lightly_parallelize(X = x, FUN = append, jobs = 2)
    out1 <- lightly_parallelize_atomic(X = x, FUN = append, jobs = 2)
    out2 <- lapply(X = x, FUN = append)
    expect_identical(out0, out1)
    expect_identical(out0, out2)
    y <- gsub("_text", "", unlist(out1))
    expect_identical(x, y)
  })
})
