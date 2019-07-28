drake_context("lock")

test_with_dir("lock_environment()", {
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism
  caching <- scenario$caching
  plan <- drake_plan(
    x = try(
      assign("a", 1L, envir = parent.env(drake_envir())),
      silent = TRUE
    )
  )
  make(
    plan,
    envir = e,
    jobs = jobs,
    parallelism = parallelism,
    caching = caching,
    lock_envir = TRUE,
    verbose = 1L,
    session_info = FALSE
  )
  expect_true(inherits(readd(x), "try-error"))
  e$a <- 123
  e$plan$four <- "five"
  plan <- drake_plan(
    x = assign("a", 1, envir = parent.env(drake_envir()))
  )
  make(
    plan,
    envir = e,
    jobs = jobs,
    parallelism = parallelism,
    caching = caching,
    lock_envir = FALSE,
    verbose = 0L,
    session_info = FALSE
  )
  expect_true("x" %in% cached())
  expect_equal(readd(x), 1L)
})

test_with_dir("Try to modify a locked environment", {
  e <- new.env()
  lock_environment(e)
  plan <- drake_plan(x = {
    e$a <- 1
    2
  })
  expect_error(
    make(plan, session_info = FALSE, cache = storr::storr_environment()),
    regexp = "Self-invalidation"
  )
})

test_with_dir("unlock_environment()", {
  expect_error(
    unlock_environment(NULL),
    regexp = "use of NULL environment is defunct"
  )
  expect_error(
    unlock_environment("x"),
    regexp = "not an environment"
  )
  e <- new.env(parent = emptyenv())
  e$y <- 1
  expect_false(environmentIsLocked(e))
  assign(x = ".x", value = "x", envir = e)
  expect_equal(get(x = ".x", envir = e), "x")
  lock_environment(e)
  msg1 <- "cannot change value of locked binding"
  msg2 <- "cannot add bindings to a locked environment"
  expect_true(environmentIsLocked(e))
  assign(x = ".x", value = "y", envir = e)
  expect_equal(get(x = ".x", envir = e), "y")
  expect_error(assign(x = "y", value = "y", envir = e), regexp = msg1)
  expect_error(assign(x = "a", value = "x", envir = e), regexp = msg2)
  expect_error(assign(x = "b", value = "y", envir = e), regexp = msg2)
  unlock_environment(e)
  assign(x = ".x", value = "1", envir = e)
  assign(x = "y", value = "2", envir = e)
  assign(x = "a", value = "x", envir = e)
  expect_equal(get(x = ".x", envir = e), "1")
  expect_equal(get(x = "y", envir = e), "2")
  expect_equal(get(x = "a", envir = e), "x")
  expect_false(environmentIsLocked(e))
  unlock_environment(e)
  assign(x = "b", value = "y", envir = e)
  expect_equal(get(x = "b", envir = e), "y")
  expect_false(environmentIsLocked(e))
})
