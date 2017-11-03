drake_context("back compatibility")

test_with_dir("migrate an up-to-date project", {
  expect_null(assert_compatible_cache(NULL))
  expect_null(get_cache())
  write_v4.1.0_cache()
  expect_error(get_cache())
})
