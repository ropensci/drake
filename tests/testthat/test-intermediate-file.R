context("intermediate-file")

test_with_dir("responses to intermediate file", {
  config <- dbug()
  testrun(config)

  # check missing and then replace file exactly as before
  final0 <- readd(final, search = FALSE)
  val <- readRDS("intermediatefile.rds")
  unlink("intermediatefile.rds", force = TRUE)
  saveRDS(val, "intermediatefile.rds")
  testrun(config)
  nobuild(config)
  expect_equal(final0, readd(final, search = FALSE))

  # actually change file
  saveRDS(sum(val) + 1, "intermediatefile.rds")
  testrun(config)
  expect_equal(justbuilt(config), "'intermediatefile.rds'")
  expect_equal(final0, readd(final, search = FALSE))

  # break the intermediate file
  unlink("intermediatefile.rds", force = TRUE)
  testrun(config)
  expect_equal(justbuilt(config), "'intermediatefile.rds'")
  expect_equal(final0, readd(final, search = FALSE))
})
