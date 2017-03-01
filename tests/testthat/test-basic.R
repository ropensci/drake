context("basic")

test_that("basic build", {
  dclean()
  args = dbug()
  expect_equal(cached(), character(0))
  run(args$plan, verbose = FALSE)
  

})
