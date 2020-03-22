drake_context("s3")

test_with_dir("S3 drake_deps (#1183)", {
  x <- new_drake_deps()
  expect_silent(validate_drake_deps(x))
  x$globals <- NULL
  expect_error(validate_drake_deps(x))
})

test_with_dir("S3 drake_deps_ht (#1183)", {
  x <- new_drake_deps_ht()
  expect_silent(validate_drake_deps_ht(x))
  x$globals <- NULL
  expect_error(validate_drake_deps_ht(x))
})

test_with_dir("S3 drake_settings (#1193)", {
  x <- drake_settings()
  expect_silent(validate_drake_settings(x))
  x$parallelism <- NULL
  expect_error(validate_drake_settings(x))
})

test_with_dir("S3 drake_triggers (#1193)", {
  x <- new_drake_triggers()
  expect_silent(validate_drake_triggers(x))
  x$command <- NULL
  expect_error(validate_drake_triggers(x))
})
