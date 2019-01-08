drake_context("memory")

test_with_dir("manage_memory() warns if loading missing deps", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  con <- drake_config(
    drake_plan(a = 1, b = a),
    memory_strategy = "lookahead"
  )
  capture.output(
    manage_memory(targets = "b", config = con),
    type = "message"
  )
  expect_false(exists("b", envir = con$eval, inherits = FALSE))
})

test_with_dir("manage_memory in full build", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  # drake_plan with lots of nested deps This will fail if
  # manage_memory() doesn't work.
  datasets <- drake_plan(x = 1, y = 2, z = 3)
  methods <- drake_plan(
    a = dataset__,
    b = dataset__,
    c = dataset__
  )
  analyses <- plan_analyses(methods, datasets)
  heuristics <- drake_plan(
    s = c(dataset__, analysis__),
    t = analysis__)
  summaries <- plan_summaries(
    heuristics,
    datasets = datasets,
    analyses = analyses,
    gather = c("c", "c")
  )
  output <- drake_plan(
    final1 = mean(s) + mean(t),
    final2 = mean(s) - mean(t),
    waitforme = c(a_x, c_y, s_b_x, t_a_z),
    waitformetoo = c(waitforme, y)
  )
  plan <- rbind(datasets, analyses, summaries, output)

  # set up a workspace to test manage_memory()
  # set verbose to TRUE to see log of loading
  config <- drake_config(
    plan,
    targets = plan$target,
    envir = new.env(parent = globalenv()),
    verbose = FALSE,
    memory_strategy = "lookahead"
  )

  # actually run
  testrun(config)
  expect_true(all(plan$target %in% cached()))

  # Check that the right targets are loaded and the right targets
  # are discarded
  remove(list = ls(config$eval), envir = config$eval)
  expect_equal(ls(config$eval), character(0))
  manage_memory(datasets$target, config)
  expect_equal(ls(config$eval), character(0))
  manage_memory(analyses$target, config)
  expect_equal(ls(config$eval), c("x", "y", "z"))
  manage_memory("waitforme", config)

  # keep y around for waitformetoo
  expect_equal(
    ls(config$eval),
    c("a_x", "c_y", "s_b_x", "t_a_z", "y")
  )
})

test_with_dir("all memory strategies work", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  for (memory_strategy in c("speed", "memory", "lookahead")) {
    envir <- new.env(parent = globalenv())
    cache <- storr::storr_environment()
    load_mtcars_example(envir = envir)
    make(envir$my_plan, envir = envir, cache = cache,
         session_info = FALSE, memory_strategy = memory_strategy)
    expect_true(file_store("report.md") %in% cache$list())
  }
})

test_with_dir("drake_envir() and memory strategies", {
  plan <- drake_plan(
    a = {
      i <- 1
      i
    },
    b = a,
    c = {
      saveRDS(ls(envir = drake_envir()), "ls.rds")
      b
    }
  )
  make(
    plan,
    memory_strategy = "speed",
    cache = storr::storr_environment(),
    session_info = FALSE
  )
  l <- readRDS("ls.rds")
  expect_true(all(c("a", "b") %in% l))
  expect_false(any(c("i") %in% l))
  make(
    plan,
    memory_strategy = "memory",
    cache = storr::storr_environment(),
    session_info = FALSE
  )
  l <- readRDS("ls.rds")
  expect_true(all(c("b") %in% l))
  expect_false(any(c("a", "i") %in% l))
  make(
    plan,
    memory_strategy = "lookahead",
    cache = storr::storr_environment(),
    session_info = FALSE
  )
  l <- readRDS("ls.rds")
  expect_true(all(c("b") %in% l))
  expect_false(any(c("a", "i") %in% l))
  plan <- drake_plan(
    a = {
      i <- 1
      i
    },
    b = {
      out <- a
      rm(a, envir = drake_envir())
      out
    },
    c = {
      saveRDS(ls(envir = drake_envir()), "ls.rds")
      b
    }
  )
  make(
    plan,
    memory_strategy = "speed",
    cache = storr::storr_environment(),
    session_info = FALSE
  )
  l <- readRDS("ls.rds")
  expect_true(all(c("b") %in% l))
  expect_false(any(c("a", "i", "out") %in% l))
})

test_with_dir("drake_envir() depth", {
  e <- new.env(parent = globalenv())
  plan <- drake_plan(
    large_data_1 = sample.int(1e4),
    large_data_2 = sample.int(1e4),
    subset = c(large_data_1[seq_len(10)], large_data_2[seq_len(10)]),
    summary = {
      targs <- c("large_data_1", "large_data_2")
      expect_true(all(targs %in% ls(envir = drake_envir())))
      identity(
        invisible(
          invisible(
            rm(large_data_1, large_data_2, envir = drake_envir())
          )
        )
      )
      expect_false(any(targs %in% ls(envir = drake_envir())))
      mean(subset)
    }
  )
  make(
    plan,
    envir = e,
    cache = storr::storr_environment(),
    session_info = FALSE
  )
})

test_with_dir("drake_envir() in wrong context", {
  expect_warning(
    drake_envir(),
    regexp = "should only be called inside commands"
  )
})
