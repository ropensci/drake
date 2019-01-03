drake_context("parallel")

test_with_dir("safe_jobs()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_error(safe_jobs(1:3))
  expect_true(is.numeric(safe_jobs(1)))
})

test_with_dir("check_jobs()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_error(check_jobs(NULL), regexp = "length")
  expect_error(check_jobs(-1), regexp = "jobs > 0")
  expect_error(check_jobs(c(1, 1)), regexp = "of length 1")
})

test_with_dir("check_parallelism()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_error(check_parallelism(character(0)), regexp = "length")
  expect_error(check_parallelism(-1), regexp = "character")
  expect_error(
    check_parallelism(letters[1:2]), regexp = "of length 1")
})


test_with_dir("parallel imports", {
  config <- dbug()
  config$jobs_preprocess <- 2
  make_imports(config)
  config$schedule <- imports_graph(config = config)
  process_imports_parLapply(config)
  expect_true("a" %in% cached())
  skip_on_os("windows")
  config$schedule <- imports_graph(config = config)
  process_imports_mclapply(config)
})

test_with_dir("lightly_parallelize_atomic() is correct", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  with_seed(seed = 2017, code = {
    x <- sample(LETTERS[1:3], size = 1e3, replace = TRUE)
    append <- function(x) {
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

test_with_dir("checksum functionality", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
  config$parallelism <- "loop"
  config$jobs <- 1
  config$cache <- storr::storr_environment()
  testrun(config)
  checksum <- mc_get_checksum(target = "combined", config = config)
  bad <- "askldfklhjsdfkj"
  expect_false(grepl("NA", checksum))
  expect_true(
    mc_is_good_checksum(
      target = "combined", checksum = checksum, config = config))
  expect_false(
    mc_is_good_checksum(
      target = "combined", checksum = bad, config = config))
  expect_silent(
    mc_wait_checksum(
      target = "combined", checksum = checksum, config = config, timeout = 0.1))
  expect_error(
    mc_wait_checksum(
      target = "combined", checksum = bad, config = config, timeout = 0.1))
  checksum <- mc_get_outfile_checksum(target = "combined", config = config)
  expect_silent(
    mc_wait_outfile_checksum(
      target = "combined", checksum = checksum, config = config, timeout = 0.1))
  expect_error(
    mc_wait_outfile_checksum(
      target = "combined", checksum = bad, config = config, timeout = 0.1))
})
