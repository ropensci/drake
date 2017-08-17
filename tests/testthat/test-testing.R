context("testing")

test_that("testing configs are there", {
  old_opt <- getOption("drake_test_opt")
  expect_true(is.list(test_opt()))
  expect_true(is.list(test_opts))
  set_test_opt(old_opt)
  expect_true(!is.null(getOption("drake_test_opt")))
  options(drake_test_opt = old_opt)
  expect_equal(old_opt, getOption("drake_test_opt"))
})
