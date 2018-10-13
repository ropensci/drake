drake_context("import object")

test_with_dir("responses to imported objects and functions", {
  config <- dbug()
  testrun(config)

  # change imported object
  config$envir$c <- config$envir$c + 1
  testrun(config)
  expect_equal(
    justbuilt(config),
    setdiff(sort(config$plan$target), "myinput")
  )

  # change nested function trivially
  eval(parse(text = "g <- function(y){

      h(  y)+b # comment
    }"),
    envir = config$envir
  )
  testrun(config)
  nobuild(config)

  # change nested function so that it gives the same answer
  eval(parse(text = "g <- function(y){
      h(y)+b + 1-1 - 0
    }"),
    envir = config$envir
  )
  testrun(config)
  expect_equal(justbuilt(config), sort(c("nextone", "yourinput")))

  # nested function gives different answer
  eval(parse(text = "g <- function(y){
      h(y)+b + 16
    }"),
    envir = config$envir
  )
  testrun(config)
  expect_true("final" %in% justbuilt(config))

  # test a deeper nested function
  eval(parse(text = "i <- function(x){
      2*x + sqrt(13)
    }"),
    envir = config$envir
  )
  testrun(config)
  expect_true("final" %in% justbuilt(config))

  # command depends on imported object k
  config$plan$command[2] <- "f(1+1) + k"
  config$envir$k <- 5
  testrun(config)
  final0 <- readd(final, search = FALSE)
  builds <- sort(
    c(
      "drake_target_1",
      "combined",
      "final",
      "yourinput"
    )
  )
  expect_equal(justbuilt(config), builds)

  # nothing to do
  testrun(config)
  nobuild(config)
  expect_true(identical(final0, readd(final, search = FALSE)))

  # change k
  config$envir$k <- 10
  testrun(config)
  expect_equal(justbuilt(config), builds)
  expect_false(identical(final0, readd(final, search = FALSE)))
})

test_with_dir("add a new import", {
  plan <- drake_plan(a = as.integer(sqrt(4)))
  cache <- storr::storr_environment()
  config <- make(plan, cache = cache, session_info = FALSE)
  expect_equal(justbuilt(config), "a")
  expect_equal(readd(a, cache = cache), 2L)
  sqrt <- function(x){
    x + 1L
  }
  config <- make(plan, cache = cache, session_info = FALSE)
  expect_equal(justbuilt(config), "a")
  expect_equal(readd(a, cache = cache), 5L)
})
