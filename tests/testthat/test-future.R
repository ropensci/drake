drake_context("future")

test_with_dir("future package functionality", {
  backend(future::sequential)
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
    outdated(e$my_plan, envir = e, verbose = FALSE),
    character(0)
  )
})

test_with_dir("future jobs warning", {
  pl <- workflow(x = 1, y = x)
  expect_warning(
    make(
      pl,
      parallelism = "future_lapply",
      jobs = 2,
      verbose = FALSE
    )
  )
})
