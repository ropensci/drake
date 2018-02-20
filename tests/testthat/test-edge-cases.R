drake_context("edge cases")

test_with_dir("config and make without safety checks", {
  x <- drake_plan(file = readRDS(file_in("my_file.rds")))
  expect_warning(tmp <- config(x, verbose = FALSE))
  expect_silent(
    tmp <- drake_config(x, skip_safety_checks = TRUE, verbose = FALSE))
  expect_silent(check_drake_config(config = tmp))
})

test_with_dir("Strings stay strings, not symbols", {
  expect_silent(x <- drake_plan(a = "A", strings_in_dots = "literals"))
  expect_silent(make(x, verbose = FALSE, session_info = FALSE))
})

test_with_dir("error handlers", {
  expect_equal(error_na(1), NA)
  expect_false(error_false(1))
  expect_equal(error_character0(1), character(0))
  expect_null(error_null(1))
})

test_with_dir("error when file target names do not match actual filenames", {
  expect_warning(x <- drake_plan(y = 1, file_targets = TRUE))
  expect_warning(expect_error(make(x, verbose = FALSE, session_info = FALSE)))
})

test_with_dir("clean a nonexistent cache", {
  clean(list = "no_cache")
  expect_false(file.exists(default_cache_path()))
})

test_with_dir("stringsAsFactors can be TRUE", {
  f <- function(x) {
    return(x)
  }
  myplan <- data.frame(target = "a", command = "f(\"helloworld\")",
    stringsAsFactors = TRUE)
  expect_true(is.factor(myplan$target))
  expect_true(is.factor(myplan$command))
  make(myplan, verbose = FALSE, session_info = FALSE)
  expect_equal(readd(a), "helloworld")
})

# Target/import conflicts are unpredictable. A warning should
# be enough.
test_with_dir("target conflicts with current import or another target", {
  config <- dbug()
  config$plan <- rbind(config$plan, data.frame(target = "f",
    command = "1+1"))
  expect_message(check_plan(plan = config$plan,
    envir = config$envir))
  config$plan$target <- "repeated"
  expect_error(check_plan(plan = config$plan))
})

test_with_dir("target conflicts with previous import", {
  config <- dbug()
  testrun(config)
  config$plan$command[2] <- "g(1+1)"
  config$plan <- rbind(config$plan, data.frame(target = "f",
    command = "1+1"))
  config$targets <- config$plan$target
  testrun(config)
  expect_equal(justbuilt(config), sort(c("\"intermediatefile.rds\"",
    "combined", "f", "final", "yourinput")))
})

test_with_dir("true targets can be functions", {
  generator <- function() return(function(x) {
    x + 1
  })
  plan <- drake_plan(myfunction = generator(), output = myfunction(1))
  config <- make(plan, verbose = FALSE, session_info = FALSE)
  expect_equal(readd(output), 2)
  expect_true(
    is.character(
      config$cache$get("myfunction",
      namespace = "kernels")
    )
  )
  myfunction <- readd(myfunction)
  expect_equal(myfunction(4), 5)
})
