drake_context("with processx")

test_with_dir("make(session = callr::r_vanilla)", {
  con <- dbug()
  con$envir <- dbug_envir(globalenv())
  ls1 <- ls(envir = con$envir)
  con$session <- callr::r_vanilla
  make_with_config(con)
  expect_equal(sort(justbuilt(con)), sort(con$plan$target))
  ls2 <- ls(envir = con$envir)
  expect_equal(sort(ls1), sort(ls2))
  rm_these <- setdiff(ls(envir = con$envir), ls1)
  if (length(rm_these)){
    rm(list = rm_these, envir = con$envir)
  }
})

test_with_dir("future_lapply (which uses processx)", {
  future::plan(future::sequential)
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  load_mtcars_example(envir = e)
  config <- make(
    e$my_plan,
    envir = e,
    parallelism = "future_lapply",
    jobs = 1,
    verbose = 4,
    session_info = FALSE
  )
  expect_equal(
    outdated(config),
    character(0)
  )
})
