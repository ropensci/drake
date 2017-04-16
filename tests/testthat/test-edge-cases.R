# library(testthat); library(devtools); load_all()
context("edge-cases")

test_that("circular non-DAG workflows quit in error", {
  p = plan(a = b, b = c, c = a)
  expect_error(check(p))
  expect_error(make(p))
})

# Target/import conflicts are unpredictable. A warning should be enough.
test_that("target conflicts with current import or another target", {
  dclean()
  config = dbug()
  config$plan = rbind(config$plan, data.frame(target = "f", 
    command = "1+1"))
  expect_warning(tmp <- capture.output(
    check(plan = config$plan, envir = config$envir)))
  config$plan$target = "repeated"
  expect_error(check(plan = config$plan))
})

test_that("target conflicts with previous import", {
  dclean()
  config = dbug()
  testrun(config)
  config$plan$command[2] = "g(1+1)"
  config$plan = rbind(config$plan, 
    data.frame(target = "f", command = "1+1"))
  config$targets = config$plan$target
  expect_warning(testrun(config))
  expect_equal(justbuilt(config), 
    sort(c("'intermediatefile.rds'", "combined", "f",
      "final", "yourinput")))
  dclean()
})

test_that("can use semicolons and multi-line commands", {
  dclean()
  plan = plan(list = c(x = "a<-1; a", y = "b<-2\nb"))
  make(plan, verbose = FALSE)
  expect_false(any(c("a", "b") %in% ls()))
  expect_true(all(cached(x, y, search = FALSE)))
  expect_equal(cached(search = FALSE), c("x", "y"))
  dclean()
})

test_that("true targets can be functions", {
  dclean()
  generator = function()
    return(function(x){x+1})
  plan = plan(myfunction = generator(), output = myfunction(1))
  make(plan, verbose = FALSE)
  expect_equal(readd(output), 2)
  cache = storr::storr_rds(cachepath)
  expect_true(is.list(cache$get("myfunction")))
  myfunction = readd(myfunction)
  expect_equal(myfunction(4), 5)
  dclean()
})
