context("back compatibility")

test_with_dir("back-compatible with a tiny v4.1.0 project", {
  root <- file.path("back-compatibility", "v4.1.0")
  cache <- system.file(
    file.path(root, "cache"),
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
  file.rename(from = "cache", to = ".drake")
  expect_true(file.copy(
    from = infile,
    to = getwd(),
    recursive = TRUE,
    overwrite = TRUE
  ))
  old_plan <- data.frame(
    target = "x",
    command = "my_function('my_file.rds')"
  )
  my_function <- function(x){
    x
  }
  expect_equal(
    session()$otherPkgs$drake$Version, # nolint
    "4.1.0"
  )
  expect_equal(
    outdated(old_plan, verbose = FALSE),
    character(0)
  )
})
