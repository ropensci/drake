drake_context("triggers")

test_with_dir("triggers work as expected", {
  con <- dbug()
  con$plan$trigger <- "missing"
  con <- testrun(config = con)
})
