drake_context("examples")

test_with_dir("examples are listed and written", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- drake_examples()
  expect_true(is.character(x) & length(x) > 0)
  for (i in x){
    expect_false(file.exists(i))
    drake_example(i)
    expect_true(file.exists(i))
    expect_true(file.info(i)$isdir)
    unlink(i, recursive = TRUE, force = TRUE)
  }
  expect_warning(drake_example(destination = getwd()))
})

test_with_dir("overwrites of report.Rmd handled correctly", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  load_mtcars_example(overwrite = TRUE)
  load_mtcars_example(overwrite = FALSE)
  expect_warning(load_mtcars_example(overwrite = TRUE))
  expect_false(file.exists("a"))
  load_mtcars_example(report_file = "a")
  expect_true(file.exists("a"))
})

test_with_dir("example template files", {
  expect_true(is.character(drake_hpc_template_files()))
  expect_true(length(drake_hpc_template_files()) > 0)
  expect_false(file.exists("slurm_batchtools.tmpl"))
  expect_silent(drake_hpc_template_file("slurm_batchtools.tmpl"))
  expect_true(file.exists("slurm_batchtools.tmpl"))
})
