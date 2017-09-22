context("back compatibility")

test_that("back-compatible with a tiny v4.1.0 project", {
  root <- file.path("back-compatibility", "v4.1.0")
  cache <- system.file(
    file.path(root, ".drake"),
    package = "drake",
    mustWork = TRUE
  )
  infile <- system.file(
    file.path(root, "my_file.rds"),
    package = "drake",
    mustWork = TRUE
  )
  expect_true(file.copy(
    from = cache, 
    to = getwd(),
    recursive = TRUE,
    overwrite = TRUE
  ))
  expect_true(file.copy(
    from = infile,
    to = getwd(),
    recursive = TRUE,
    overwrite = TRUE
  ))
  old_plan <- data.frame(
    target = "x",
    command = "readRDS('my_file.rds')"
  )
  expect_equal(
    session()$otherPkgs$drake$Version,
    "4.1.0"
  )
  expect_equal(
    outdated(old_plan, verbose = FALSE),
    character(0)
  )
})
