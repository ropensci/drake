drake_context("memory")

test_with_dir("manage_memory() warns if loading missing deps", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  con <- drake_config(
    drake_plan(a = 1, b = a),
    memory_strategy = "lookahead",
    garbage_collection = TRUE
  )
  con$envir_graph <- ht_new()
  con$envir_graph$graph <- con$graph
  capture.output(
    suppressWarnings( # https://github.com/richfitz/storr/issues/105 # nolint
      manage_memory(target = "b", config = con)
    ),
    type = "message"
  )
  expect_false(exists("b", envir = con$envir_targets, inherits = FALSE))
})

test_with_dir("garbage collection", {
  plan <- drake_plan(x = "x", y = "y")
  make(plan, garbage_collection = TRUE)
  expect_equal(readd(y), "y")
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
  envir <- environment()
  config <- drake_config(
    plan,
    envir = envir,
    cache = storr::storr_environment(),
    session_info = FALSE,
    memory_strategy = "lookahead"
  )
  config$envir_graph$graph <- config$graph
  config$envir_loaded <- ht_new()

  # actually run
  config$plan <- plan
  make(config = config)
  expect_true(all(plan$target %in% cached(cache = config$cache)))

  # lookahead
  clear_envir_targets("", config)
  expect_equal(ls(config$envir_targets), character(0))
  lapply(c("x", "y", "z"), function(x) {
    manage_memory(x, config)
    expect_equal(ls(config$envir_targets), character(0))
  })
  lapply(c("a_x", "b_x", "c_x"), function(x) {
    manage_memory(x, config)
    expect_equal(ls(config$envir_targets), "x")
  })
  manage_memory("a_y", config, downstream = "a_x")
  expect_equal(sort(ls(config$envir_targets)), sort(c("x", "y")))
  manage_memory("s", config)
  deps <- paste(
    "s",
    rep(letters[1:3], times = 3),
    rep(letters[24:26], each = 3),
    sep = "_"
  )
  expect_equal(sort(deps), sort(ls(config$envir_targets)))
  config$envir_targets$y <- 1
  config$envir_loaded$targets <- c(config$envir_loaded$targets, "y")
  manage_memory("waitforme", config)
  deps <- c("y", "a_x", "c_y", "t_a_z", "s_b_x")
  expect_equal(sort(deps), sort(ls(config$envir_targets)))
  manage_memory("waitforme", config, downstream = "waitforme")
  deps <- c("a_x", "c_y", "t_a_z", "s_b_x")
  expect_equal(sort(deps), sort(ls(config$envir_targets)))

  # speed
  config$memory_strategy <- "speed"
  clear_envir_targets("", config)
  expect_equal(ls(config$envir_targets), character(0))
  lapply(c("x", "y", "z"), function(x) {
    manage_memory(x, config)
    expect_equal(ls(config$envir_targets), character(0))
  })
  lapply(c("a_x", "b_x", "c_x"), function(x) {
    manage_memory(x, config)
    expect_equal(ls(config$envir_targets), "x")
  })
  manage_memory("a_y", config)
  expect_equal(sort(ls(config$envir_targets)), sort(c("x", "y")))
  manage_memory("s", config)
  deps <- paste(
    "s",
    rep(letters[1:3], times = 3),
    rep(letters[24:26], each = 3),
    sep = "_"
  )
  deps <- c(deps, c("x", "y"))
  expect_equal(sort(deps), sort(ls(config$envir_targets)))
  config$envir_targets$y <- 1
  config$envir_loaded$targets <- c(config$envir_loaded$targets, "y")
  manage_memory("waitforme", config)
  deps <- unique(c(deps, "a_x", "c_y", "t_a_z", "s_b_x"))
  expect_equal(sort(deps), sort(ls(config$envir_targets)))

  # autoclean and preclean
  for (strategy in c("preclean", "autoclean")) {
    config$memory_strategy <- strategy
    rm(list = names(config$envir_targets), envir = config$envir_targets)
    config$envir_loaded$targets <- character(0)

    # initial discard and load
    expect_equal(ls(config$envir_targets), character(0))
    lapply(c("x", "y", "z"), function(x) {
      manage_memory(x, config)
      expect_equal(ls(config$envir_targets), character(0))
    })
    lapply(c("a_x", "b_x", "c_x"), function(x) {
      manage_memory(x, config)
      expect_equal(ls(config$envir_targets), "x")
    })
    manage_memory("a_y", config)
    expect_equal(ls(config$envir_targets), "y")
    manage_memory("s", config)
    deps <- paste(
      "s",
      rep(letters[1:3], times = 3),
      rep(letters[24:26], each = 3),
      sep = "_"
    )
    expect_equal(sort(deps), sort(ls(config$envir_targets)))
    config$envir_targets$y <- 1
    config$envir_loaded$targets <- c(config$envir_loaded$targets, "y")
    manage_memory("waitforme", config)
    deps <- c("a_x", "c_y", "t_a_z", "s_b_x")
    expect_equal(sort(deps), sort(ls(config$envir_targets)))

    # final discard
    if (exists("x", envir = config$envir_targets, inherits = FALSE)) {
      rm(list = "x", envir = config$envir_targets)
    }
    config$envir_loaded$targets <- setdiff(config$envir_loaded$targets, "x")
    assign_to_envir("x", "value", config)
    e <- exists("x", envir = config$envir_targets, inherits = FALSE)
    e2 <- "x" %in% config$envir_loaded$targets
    expect_equal(e, config$memory_strategy == "preclean")
    expect_equal(e, e2)
    if (e) {
      rm(list = "x", envir = config$envir_targets)
      config$envir_loaded$targets <- setdiff(config$envir_loaded$targets, "x")
    }
  }

  # none
  config$memory_strategy <- "none"
  manage_memory("final1", config)
  expect_equal(sort(deps), sort(ls(config$envir_targets)))

  # unload
  config$memory_strategy <- "unload"
  manage_memory("s", config)
  expect_equal(ls(config$envir_targets), character(0))
})

test_with_dir("primary memory strategies actually build everything", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("knitr")
  for (memory_strategy in c("speed", "autoclean", "preclean", "lookahead")) {
    envir <- new.env(parent = globalenv())
    cache <- storr::storr_environment()
    load_mtcars_example(envir = envir)
    make(
      envir$my_plan,
      envir = envir,
      cache = cache,
      session_info = FALSE,
      memory_strategy = memory_strategy
    )
    expect_true(file_store("report.md") %in% cache$list())
  }
})

test_with_dir("The unload and none strategies do not hold on to targets", {
  for (mem in c("none", "unload")) {
    plan <- drake_plan(target84d2fe31 = 1, target167ff309 = target84d2fe31)
    expect_error(make(plan, memory_strategy = mem), regexp = "not found")
    expect_equal(failed(), "target167ff309")
    clean()
    plan <- drake_plan(
      target84d2fe31 = 1,
      target167ff309 = readd(target84d2fe31)
    )
    clean()
    make(plan, memory_strategy = mem)
    config <- drake_config(plan, garbage_collection = TRUE)
    expect_equal(sort(justbuilt(config)), sort(plan$target))
    rm(envir = config$envir_targets)
    rm(envir = config$cache$envir)
    clean(destroy = TRUE, cache = config$cache)
    gc()
  }
})

test_with_dir("different memory strategies for each targret", {
  plan <- drake_plan(
    targetx84d2fe31 = 1,
    targetx167ff309 = target(targetx84d2fe31, memory_strategy = "speed"),
    targetx523e1fba = target(targetx167ff309, memory_strategy = "unload")
  )
  expect_error(make(plan), regexp = "not found")
  expect_equal(failed(), "targetx523e1fba")
})

test_with_dir("drake_envir() and memory strategies", {
  plan <- drake_plan(
    targety84d2fe31 = {
      i <- 1
      i
    },
    targety167ff309 = targety84d2fe31,
    targety523e1fba = {
      saveRDS(ls(envir = parent.env(drake_envir())), "ls.rds")
      targety167ff309
    }
  )
  make(
    plan,
    memory_strategy = "speed",
    cache = storr::storr_environment(),
    session_info = FALSE
  )
  l <- readRDS("ls.rds")
  expect_true(all(c("targety84d2fe31", "targety167ff309") %in% l))
  expect_false(any(c("i") %in% l))
  make(
    plan,
    memory_strategy = "preclean",
    cache = storr::storr_environment(),
    session_info = FALSE
  )
  l <- readRDS("ls.rds")
  expect_true(all(c("targety167ff309") %in% l))
  expect_false(any(c("targety84d2fe31", "i") %in% l))
  make(
    plan,
    memory_strategy = "lookahead",
    cache = storr::storr_environment(),
    session_info = FALSE
  )
  l <- readRDS("ls.rds")
  expect_true(all(c("targety167ff309") %in% l))
  expect_false(any(c("targety84d2fe31", "i") %in% l))
  plan <- drake_plan(
    targety84d2fe31 = {
      i <- 1
      i
    },
    targety167ff309 = {
      out <- targety84d2fe31
      rm(targety84d2fe31, envir = parent.env(drake_envir()))
      out
    },
    c = {
      saveRDS(ls(envir = parent.env(drake_envir())), "ls.rds")
      targety167ff309
    }
  )
  make(
    plan,
    memory_strategy = "speed",
    cache = storr::storr_environment(),
    session_info = FALSE
  )
  l <- readRDS("ls.rds")
  expect_true(all(c("targety167ff309") %in% l))
  expect_false(any(c("targety84d2fe31", "i", "out") %in% l))
})

test_with_dir("drake_envir() depth", {
  e <- new.env(parent = globalenv())
  plan <- drake_plan(
    large_data_1 = sample.int(1e4),
    large_data_2 = sample.int(1e4),
    subset = c(large_data_1[seq_len(10)], large_data_2[seq_len(10)]),
    summary = {
      targs <- c("large_data_1", "large_data_2")
      expect_true(all(targs %in% ls(
        envir = parent.env(drake_envir())
      )))
      identity(
        invisible(
          invisible(
            rm(large_data_1, large_data_2, envir = parent.env(drake_envir()))
          )
        )
      )
      expect_false(any(targs %in% ls(envir = parent.env(drake_envir()))))
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
    regexp = "in your drake plan"
  )
})
