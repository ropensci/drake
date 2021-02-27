drake_context("missed")

test_with_dir("missed_impl() works with in-memory deps", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  # May have been loaded in a globalenv() testing scenario
  remove_these <- intersect(ls(envir = globalenv()), c("f", "g"))
  rm(list = remove_these, envir = globalenv())
  o <- dbug()
  expect_equal(character(0), missed_impl(o))
  rm(list = c("f", "g"), envir = o$envir)
  expect_equal(sort(c("f", "g")), sort(missed_impl(o)))
})

test_with_dir("missed_impl() works with files", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  o <- dbug()
  expect_equal(character(0), missed_impl(o))
  unlink("input.rds")
  expect_equal(redisplay_keys(reencode_path("input.rds")), missed_impl(o))
})
