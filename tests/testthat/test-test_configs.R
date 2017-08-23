context("test_configs")

test_that("at least one test to run on CRAN", {
  expect_true(any(unlist(lapply(drake:::test_configs(), `[[`, "cran"))))
})
