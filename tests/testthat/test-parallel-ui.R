context("parallel-ui")

test_with_dir("shell_file() writes correctly", {
  expect_false(file.exists("shell.sh"))
  suppressWarnings(shell_file()) # chmod may not work on Windows
  expect_true(file.exists("shell.sh"))
  unlink("shell.sh", force = TRUE)
  d <- "exdir"
  dir.create(d)
  p <- file.path(d, "script.txt")
  expect_false(file.exists(p))
  suppressWarnings(shell_file(p)) # chmod may not work on Windows
  expect_true(file.exists(p))
  unlink(d, recursive = TRUE, force = TRUE)
})

test_with_dir("mclapply and lapply", {
  config <- dbug()
  make(plan = config$plan, envir = config$envir, verbose = FALSE,
    jobs = 1, parallelism = "mclapply")
  expect_true(is.numeric(readd(final)))
  clean()
  make(plan = config$plan, envir = config$envir, verbose = FALSE,
    jobs = 1, parallelism = "parLapply")
  expect_true(is.numeric(readd(final)))
})
