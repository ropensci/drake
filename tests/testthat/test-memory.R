drake_context("memory")

test_with_dir("manage_memory() warns if loading missing deps", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  con <- drake_config(
    drake_plan(a = 1, b = a),
    memory_strategy = "lookahead"
  )
  capture.output(
    suppressWarnings( # https://github.com/richfitz/storr/issues/105 # nolint
      manage_memory(target = "b", config = con)
    ),
    type = "message"
  )
  expect_false(exists("b", envir = con$eval, inherits = FALSE))
})

test_with_dir("a close look at the memory strategies", {
  plan <- drake_plan(
    x = 1,
    y = 2,
    z = 3,
    a_x = x,
    a_y = y,
    a_z = z,
    b_x = x,
    b_y = y,
    b_z = z,
    c_x = x,
    c_y = y,
    c_z = z,
    s = c(
      s_a_x = s_a_x, s_a_y = s_a_y, s_a_z = s_a_z, s_b_x = s_b_x,
      s_b_y = s_b_y, s_b_z = s_b_z, s_c_x = s_c_x, s_c_y = s_c_y,
      s_c_z = s_c_z
    ),
    t = c(
      t_a_x = t_a_x, t_a_y = t_a_y, t_a_z = t_a_z, t_b_x = t_b_x,
      t_b_y = t_b_y, t_b_z = t_b_z, t_c_x = t_c_x, t_c_y = t_c_y,
      t_c_z = t_c_z
    ),
    s_a_x = c(x, a_x),
    s_a_y = c(y, a_y),
    s_a_z = c(z, a_z),
    s_b_x = c(x, b_x),
    s_b_y = c(y, b_y),
    s_b_z = c(z, b_z),
    s_c_x = c(x, c_x),
    s_c_y = c(y, c_y),
    s_c_z = c(z, c_z),
    t_a_x = a_x,
    t_a_y = a_y,
    t_a_z = a_z,
    t_b_x = b_x,
    t_b_y = b_y,
    t_b_z = b_z,
    t_c_x = c_x,
    t_c_y = c_y,
    t_c_z = c_z,
    final1 = mean(s) + mean(t),
    final2 = mean(s) - mean(t),
    waitforme = c(a_x, c_y, s_b_x, t_a_z),
    waitformetoo = c(waitforme, y)
  )
  config <- drake_config(
    plan,
    cache = storr::storr_environment(),
    session_info = FALSE,
    memory_strategy = "lookahead"
  )

  # actually run
  config$plan <- plan
  make(config = config)
  expect_true(all(plan$target %in% cached(cache = config$cache)))

  # lookahead
  remove(list = ls(config$eval), envir = config$eval)
  expect_equal(ls(config$eval), character(0))
  for (x in c("x", "y", "z")) {
    manage_memory(x, config)
    expect_equal(ls(config$eval), character(0))
  }
  for (x in c("a_x", "b_x", "c_x")) {
    manage_memory(x, config)
    expect_equal(ls(config$eval), "x")
  }
  manage_memory("a_y", config, downstream = "a_x")
  expect_equal(sort(ls(config$eval)), sort(c("x", "y")))
  manage_memory("s", config)
  deps <- paste(
    "s",
    rep(letters[1:3], times = 3),
    rep(letters[24:26], each = 3),
    sep = "_"
  )
  expect_equal(sort(deps), sort(ls(config$eval)))
  config$eval$y <- 1
  manage_memory("waitforme", config)
  deps <- c("y", "a_x", "c_y", "t_a_z", "s_b_x")
  expect_equal(sort(deps), sort(ls(config$eval)))
  manage_memory("waitforme", config, downstream = "waitforme")
  deps <- c("a_x", "c_y", "t_a_z", "s_b_x")
  expect_equal(sort(deps), sort(ls(config$eval)))

  # speed
  config$memory_strategy <- "speed"
  remove(list = ls(config$eval), envir = config$eval)
  expect_equal(ls(config$eval), character(0))
  for (x in c("x", "y", "z")) {
    manage_memory(x, config)
    expect_equal(ls(config$eval), character(0))
  }
  for (x in c("a_x", "b_x", "c_x")) {
    manage_memory(x, config)
    expect_equal(ls(config$eval), "x")
  }
  manage_memory("a_y", config)
  expect_equal(sort(ls(config$eval)), sort(c("x", "y")))
  manage_memory("s", config)
  deps <- paste(
    "s",
    rep(letters[1:3], times = 3),
    rep(letters[24:26], each = 3),
    sep = "_"
  )
  deps <- c(deps, c("x", "y"))
  expect_equal(sort(deps), sort(ls(config$eval)))
  config$eval$y <- 1
  manage_memory("waitforme", config)
  deps <- unique(c(deps, "a_x", "c_y", "t_a_z", "s_b_x"))
  expect_equal(sort(deps), sort(ls(config$eval)))

  # memory
  config$memory_strategy <- "memory"
  remove(list = ls(config$eval), envir = config$eval)
  expect_equal(ls(config$eval), character(0))
  for (x in c("x", "y", "z")) {
    manage_memory(x, config)
    expect_equal(ls(config$eval), character(0))
  }
  for (x in c("a_x", "b_x", "c_x")) {
    manage_memory(x, config)
    expect_equal(ls(config$eval), "x")
  }
  manage_memory("a_y", config)
  expect_equal(ls(config$eval), "y")
  manage_memory("s", config)
  deps <- paste(
    "s",
    rep(letters[1:3], times = 3),
    rep(letters[24:26], each = 3),
    sep = "_"
  )
  expect_equal(sort(deps), sort(ls(config$eval)))
  config$eval$y <- 1
  manage_memory("waitforme", config)
  deps <- c("a_x", "c_y", "t_a_z", "s_b_x")
  expect_equal(sort(deps), sort(ls(config$eval)))

  # none
  config$memory_strategy <- "none"
  manage_memory("final1", config)
  expect_equal(sort(deps), sort(ls(config$eval)))

  # unload
  config$memory_strategy <- "unload"
  manage_memory("s", config)
  expect_equal(ls(config$eval), character(0))
})

test_with_dir("primary memory strategies actually build everything", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("knitr")
  for (memory_strategy in c("speed", "memory", "lookahead")) {
    envir <- new.env(parent = globalenv())
    cache <- storr::storr_environment()
    load_mtcars_example(envir = envir)
    make(envir$my_plan, envir = envir, cache = cache,
         session_info = FALSE, memory_strategy = memory_strategy)
    expect_true(file_store("report.md") %in% cache$list())
  }
})

test_with_dir("The unload and none strategies do not hold on to targets", {
  for (mem in c("none", "unload")) {
    plan <- drake_plan(a = 1, b = a)
    expect_error(make(plan, memory_strategy = mem), regexp = "not found")
    expect_equal(failed(), "b")
    clean()
    plan <- drake_plan(a = 1, b = readd(a))
    clean()
    make(plan, memory_strategy = mem)
    config <- drake_config(plan)
    expect_equal(sort(justbuilt(config)), sort(plan$target))
    rm(envir = config$eval)
    rm(envir = config$cache$envir)
    clean(destroy = TRUE, cache = config$cache)
    gc()
  }
})

test_with_dir("different memory strategies for each targret", {
  plan <- drake_plan(
    a = 1,
    b = target(a, memory_strategy = "speed"),
    c = target(b, memory_strategy = "unload")
  )
  expect_error(make(plan), regexp = "not found")
  expect_equal(failed(), "c")
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
  expect_error(
    drake_envir(),
    regexp = "should only be called inside commands"
  )
})
