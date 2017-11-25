drake_context("future")

test_with_dir("future package functionality", {
  future::plan(future::sequential)
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  load_basic_example(envir = e)
  expect_silent(
    withr::with_options(
      new = list(mc.cores = 2), code = {
        config <- make(
          e$my_plan,
          envir = e,
          parallelism = "future_lapply",
          jobs = 1,
          verbose = FALSE
        )
      }
    )
  )
  expect_equal(
    outdated(config),
    character(0)
  )
})
