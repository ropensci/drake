drake_context("future")

test_with_dir("future", {
  backend(future::multisession)
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  load_basic_example(envir = e)
  withr::with_options(
    new = list(mc.cores = 2), code = {
      config <- make(
        e$my_plan,
        envir = e,
        parallelism = "future_lapply",
        jobs = 2,
        verbose = FALSE
      )
    }
  )
  expect_equal(
    outdated(e$my_plan, envir = e, verbose = FALSE),
    character(0)
  )
})

test_with_dir("workflow", {
  expect_equal(
    drake::plan(x = 1, y = "f()"),
    workflow(x = 1, y = "f()")
  )
})
