context("testing")

test_with_dir("testing configs are there", {
  dclean()
  old_opt <- getOption("drake_test_opt")
  expect_true(is.list(test_opt()))
  expect_true(is.list(test_opts))
  expect_true(is.character(capture.output(show_config_opts(dbug()$config))))
  set_test_opt(old_opt)
  expect_true(!is.null(getOption("drake_test_opt")))
  options(drake_test_opt = old_opt)
  expect_equal(old_opt, getOption("drake_test_opt"))
  dclean()
})
