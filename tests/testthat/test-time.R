context("time")

test_with_dir("build times works if no targets are built", {
  expect_equal(nrow(build_times(search = FALSE)), 0)
  my_plan <- plan(x = 1)
  make(my_plan, verbose = FALSE, imports_only = TRUE)
  expect_equal(nrow(build_times(search = FALSE)), 0)
})
