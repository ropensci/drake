drake_context("edge cases")

test_with_dir("error handlers", {
  expect_equal(error_na(1), NA)
  expect_false(error_false(1))
  expect_equal(error_character0(1), character(0))
})

test_with_dir("error when file target names do not match actual filenames", {
  x <- workplan(y = 1, file_targets = TRUE)
  expect_warning(expect_error(make(x, verbose = FALSE)))
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
  make(myplan, verbose = FALSE)
  expect_equal(readd(a), "helloworld")
})

# Target/import conflicts are unpredictable. A warning should
# be enough.
test_with_dir("target conflicts with current import or another target", {
  config <- dbug()
  config$plan <- rbind(config$plan, data.frame(target = "f",
    command = "1+1"))
  expect_message(check(plan = config$plan,
    envir = config$envir))
  config$plan$target <- "repeated"
  expect_error(check(plan = config$plan))
})

test_with_dir("target conflicts with previous import", {
  config <- dbug()
  testrun(config)
  config$plan$command[2] <- "g(1+1)"
  config$plan <- rbind(config$plan, data.frame(target = "f",
    command = "1+1"))
  config$targets <- config$plan$target
  testrun(config)
  expect_equal(justbuilt(config), sort(c("'intermediatefile.rds'",
    "combined", "f", "final", "yourinput")))
})

test_with_dir("can use semicolons and multi-line commands", {
  plan <- workplan(list = c(x = "a<-1; a", y = "b<-2\nb"))
  make(plan, verbose = FALSE)
  expect_false(any(c("a", "b") %in% ls()))
  expect_true(all(cached(x, y, search = FALSE)))
  expect_equal(cached(search = FALSE), c("x", "y"))
})

test_with_dir("true targets can be functions", {
  generator <- function() return(function(x) {
    x + 1
  })
  plan <- workplan(myfunction = generator(), output = myfunction(1))
  config <- make(plan, verbose = FALSE)
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

test_with_dir("deprecation", {
  expect_warning(default_system2_args(jobs = 1, verbose = FALSE))
  plan <- data.frame(code = 1:2, output = c("x", "y"))
  expect_warning(make(plan, verbose = FALSE))
  expect_warning(make(plan, verbose = FALSE))
  expect_warning(status())
  expect_true(is.numeric(readd(x, search = FALSE)))
  expect_warning(prune(plan[1, ]))
  expect_equal(cached(), "x")
  expect_warning(make(workplan(x = 1), return_config = TRUE,
                      verbose = FALSE))
  expect_warning(make(workplan(x = 1), clear_progress = TRUE,
                      verbose = FALSE))
  expect_silent(make(workplan(x = 1), imports_only = TRUE,
                     verbose = FALSE))
  pl1 <- expect_warning(drake::plan(x = 1, y = x))
  pl2 <- workplan(x = 1, y = x)
  expect_warning(drake::plan())
  expect_warning(drake::plan(x = y, file_targets = TRUE))
  expect_warning(drake::workflow())
  expect_warning(drake::workflow(x = y, file_targets = TRUE))
  
  # We need to be able to set the drake version
  # to check back compatibility.
  x <- this_cache()
  x$del(key = "initial_drake_version", namespace = "session")
  expect_false("initial_drake_version" %in% x$list(namespace = "session"))
  set_initial_drake_version(cache = x)
  expect_true("initial_drake_version" %in% x$list(namespace = "session"))
})
