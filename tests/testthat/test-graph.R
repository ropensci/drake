drake_context("graph")

test_with_dir("Recursive functions are okay", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  factorial <- function(n) {
    if (n == 0) {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  x <- drake_plan(output = factorial(10))
  cache <- storr::storr_environment()
  make(x, cache = cache, session_info = FALSE)
  expect_equal(readd(output, cache = cache), factorial(10))
})

test_with_dir("null graph", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("lubridate")
  skip_if_not_installed("visNetwork")
  x <- drake_graph_info(config = list(graph = igraph::make_empty_graph()))
  expect_equal(x, null_graph())
})

test_with_dir("circular non-DAG drake_plans quit in error", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- drake_plan(a = b, b = c, c = a)
  expect_error(tmp <- capture.output(check_plan_(x)))
  expect_error(
    make(x, verbose = FALSE, session_info = FALSE),
    regexp = "[Cc]ircular workflow"
  )
  x <- drake_plan(
    a = b, b = c, c = a, d = 4, e = d,
    Aa = Bb, Bb = Cc, Cc = Aa, mytarget = e
  )
  expect_error(tmp <- capture.output(check_plan_(x)))
  expect_error(
    make(x, verbose = FALSE, session_info = FALSE),
    regexp = "[Cc]ircular workflow"
  )
})

test_with_dir("Supplied graph disagrees with the workflow plan", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  con <- dbug()
  con2 <- drake_config(drake_plan(a = 1), verbose = FALSE)
  expect_warning(
    make(
      plan = con$plan,
      envir = con$envir,
      graph = con2$graph,
      verbose = FALSE,
      session_info = FALSE
    )
  )
})

test_with_dir("we can generate different visNetwork dependency graphs", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("lubridate")
  skip_if_not_installed("visNetwork")
  load_mtcars_example()
  config <- drake_config(my_plan)

  # Different graph configurations should be checked visually.
  expect_warning(
    tmp <- drake_graph_info(
      config = config, build_times = FALSE, from_scratch = TRUE))
  expect_warning(
    tmp <- drake_graph_info(
      config = config, build_times = FALSE, full_legend = TRUE))
  expect_warning(
    tmp <- drake_graph_info(config = config, build_times = TRUE))
  expect_warning(
    tmp <- drake_graph_info(config = config, build_times = FALSE))
  tmpcopy <- drake_graph_info(config = config,
    make_imports = FALSE, build_times = "none")
  tmp0 <- drake_graph_info(config = config, build_times = "none",
    subset = c("small", "regression2_large"))
  tmp1 <- drake_graph_info(config = config, build_times = "none",
    from = "small")
  tmp2 <- drake_graph_info(config = config, build_times = "none",
    from = "small", targets_only = TRUE)
  tmp3 <- drake_graph_info(config = config, build_times = "none",
    targets_only = TRUE)
  tmp4 <- drake_graph_info(config = config, build_times = "none",
    targets_only = TRUE)
  tmp5 <- drake_graph_info(config = config, build_times = "build",
    targets_only = TRUE)
  tmp6 <- drake_graph_info(config = config, build_times = "build",
    targets_only = TRUE, from_scratch = FALSE)
  expect_error(
    tmp7 <- drake_graph_info(config = config, build_times = "none",
                             from = "not_found")
  )
  expect_equal(nrow(tmp0$nodes), 2)
  expect_true(identical(tmp$nodes, tmpcopy$nodes))
  expect_false(identical(tmp$nodes, tmp0$nodes))
  expect_false(identical(tmp$nodes, tmp1$nodes))
  expect_false(identical(tmp$nodes, tmp2$nodes))
  expect_false(identical(tmp$nodes, tmp3$nodes))
  expect_false(identical(tmp$nodes, tmp4$nodes))
  expect_false(identical(tmp$nodes, tmp5$nodes))
  expect_false(identical(tmp$nodes, tmp6$nodes))

  expect_false(file.exists("Makefile"))
  expect_true(is.data.frame(tmp$nodes))
  expect_equal(sort(outdated(config = config)),
               sort(c(config$plan$target)))
  expect_false(file.exists("Makefile"))
})

test_with_dir("clusters", {
  skip_on_cran()
  plan <- drake_plan(x = rnorm(n__), y = rexp(n__))
  plan <- evaluate_plan(plan, wildcard = "n__", values = 1:2, trace = TRUE)
  cache <- storr::storr_environment()
  config <- drake_config(plan, cache = cache)
  skip_if_not_installed("lubridate")
  skip_if_not_installed("visNetwork")
  o1 <- drake_graph_info(config)
  o1$nodes$level <- as.integer(o1$nodes$level)
  o2 <- drake_graph_info(config, group = "n__", clusters = "asdfae")
  o3 <- drake_graph_info(config, group = "n__")
  o4 <- drake_graph_info(config, group = "adfe")
  for (col in c("label", "deps", "trigger")) {
    o1$nodes[[col]] <-
      o2$nodes[[col]] <-
      o3$nodes[[col]] <-
      o4$nodes[[col]] <-
      NULL
  }
  expect_equal(o1$nodes, o2$nodes)
  expect_equal(o1$nodes, o3$nodes)
  expect_equal(o1$nodes, o4$nodes)
  o <- drake_graph_info(config, group = "n__", clusters = "1")
  expect_equal(nrow(o$nodes), 3)
  expect_equal(
    sort(o$nodes$id),
    sort(c("x_2", "y_2", "n__: 1"))
  )
  node <- o$nodes[o$nodes$id == "n__: 1", ]
  expect_equal(node$id, "n__: 1")
  expect_equal(node$type, "cluster")
  expect_equal(node$shape, unname(shape_of("cluster")))
  o <- drake_graph_info(config, group = "n__", clusters = c("1", "2", "bla"))
  expect_equal(nrow(o$nodes), 2)
  expect_equal(
    sort(o$nodes$id),
    sort(c("n__: 1", "n__: 2"))
  )
  for (x in c("n__: 1", "n__: 2")) {
    node <- o$nodes[o$nodes$id == x, ]
    expect_equal(node$id, x)
    expect_equal(node$type, "cluster")
    expect_equal(node$shape, unname(shape_of("cluster")))
  }
  make(plan, targets = c("x_1", "y_2"), cache = cache, session_info = FALSE)
  o <- drake_graph_info(config, group = "status", clusters = "up to date")
  expect_equal(nrow(o$nodes), 3)
  expect_equal(
    sort(o$nodes$id),
    sort(c("x_2", "y_1", "status: up to date"))
  )
  node <- o$nodes[o$nodes$id == "status: up to date", ]
  expect_equal(node$id, "status: up to date")
  expect_equal(node$type, "cluster")
  expect_equal(node$shape, unname(shape_of("cluster")))
})

test_with_dir("can get the graph info when a file is missing", {
  skip_on_cran()
  load_mtcars_example()
  unlink("report.Rmd")
  expect_warning(
    config <- drake_config(
      my_plan,
      cache = storr::storr_environment(),
      session_info = FALSE
    )
  )
  skip_if_not_installed("lubridate")
  skip_if_not_installed("visNetwork")
  suppressWarnings(o <- drake_graph_info(config))
  expect_true("missing" %in% o$nodes$status)
})

test_with_dir("misc graph utils", {
  expect_true(is.character(default_graph_title()))
})

test_with_dir("file_out()/file_in() connections", {
  plan <- drake_plan(
    out1 = saver1,
    saver1 = file_out("a", "b", "c"),
    reader3 = file_in("b", "d"),
    saver2 = file_out("d"),
    out2 = reader2,
    reader1 = file_in("c", "d"),
    reader2 = file_in("a", "b")
  )
  config <- drake_config(
    plan,
    session_info = FALSE,
    cache = storr::storr_environment()
  )
  dependencies <- function(target, config, reverse = FALSE) {
    deps_graph(targets = target, graph = config$graph, reverse = reverse)
  }
  expect_equal(dependencies("out1", config), "saver1")
  expect_equal(dependencies("saver1", config), character(0))
  expect_equal(
    sort(dependencies("reader3", config)),
    sort(c("saver1", "saver2", encode_path("b"), encode_path("d")))
  )
  expect_equal(dependencies("saver2", config), character(0))
  expect_equal(dependencies("out2", config), "reader2")
  expect_equal(
    sort(dependencies("reader1", config)),
    sort(c("saver1", "saver2", encode_path("c"), encode_path("d")))
  )
  expect_equal(
    sort(dependencies("reader2", config)),
    sort(c("saver1", encode_path("a"), encode_path("b")))
  )
  expect_equal(dependencies("out1", config, reverse = TRUE), character(0))
  expect_equal(
    sort(dependencies("saver1", config, reverse = TRUE)),
    sort(c("out1", "reader1", "reader2", "reader3",
           encode_path("a"), encode_path("b"), encode_path("c")))
  )
  expect_equal(dependencies("reader3", config, reverse = TRUE), character(0))
  expect_equal(
    sort(dependencies("saver2", config, reverse = TRUE)),
    sort(c("reader1", "reader3", encode_path("d")))
  )
  expect_equal(dependencies("out2", config, reverse = TRUE), character(0))
  expect_equal(dependencies("reader1", config, reverse = TRUE), character(0))
  expect_equal(dependencies("reader2", config, reverse = TRUE), "out2")
})

# GitHub issue 486
test_with_dir("show_output_files", {
  skip_on_cran()
  plan <- drake_plan(
    target1 = {
      file_in("in1.txt", "in2.txt")
      file_out("out1.txt", "out2.txt")
      file.create("out1.txt")
      file.create("out2.txt")
    },
    target2 = {
      file_in("out1.txt", "out2.txt")
      file_out("out3.txt", "out4.txt")
      file.create("out3.txt")
      file.create("out4.txt")
    }
  )
  writeLines("in1", "in1.txt")
  writeLines("in2", "in2.txt")
  cache <- storr::storr_environment()
  make(
    plan,
    cache = cache,
    session_info = FALSE
  )
  config <- drake_config(
    plan,
    cache = cache,
    session_info = FALSE
  )
  writeLines("abcdefg", "out3.txt")
  expect_equal(outdated(config), "target2")
  skip_if_not_installed("lubridate")
  skip_if_not_installed("visNetwork")
  info <- drake_graph_info(
    config,
    show_output_files = TRUE,
    targets_only = TRUE
  )
  expect_equal(
    sort(info$nodes$id),
    sort(c(plan$target, file_store(paste0("out", 1:4, ".txt"))))
  )
  for (file in paste0("out", 1:2, ".txt")) {
    expect_equal(
      info$nodes[info$nodes$id == file_store(file), ]$status,
      "up to date"
    )
  }
  for (file in paste0("out", 3:4, ".txt")) {
    expect_equal(
      info$nodes[info$nodes$id == file_store(file), ]$status,
      "outdated"
    )
  }
  e <- info$edges[with(info$edges, order(from, to)), ]
  expect_equal(
    sort(e$from),
    sort(c(
      file_store(paste0("out", 1:2, ".txt")),
      "target1",
      paste0("target", rep(1:2, each = 2))
    ))
  )
  expect_equal(
    sort(e$to),
    sort(c(
      rep("target2", 3),
      file_store(paste0("out", 1:4, ".txt"))
    ))
  )
  info <- drake_graph_info(
    config,
    show_output_files = FALSE,
    targets_only = TRUE
  )
  expect_equal(sort(info$nodes$id), sort(paste0("target", 1:2)))
  expect_equal(info$edges$from, "target1")
  expect_equal(info$edges$to, "target2")
  info <- drake_graph_info(
    config,
    show_output_files = TRUE,
    targets_only = TRUE,
    group = "status",
    clusters = "up to date"
  )
  expect_equal(
    sort(info$nodes$id),
    sort(c(file_store(
      paste0("out", 3:4, ".txt")), "status: up to date", "target2"))
  )
})

# GitHub issue 486
test_with_dir("same, but with an extra edge not due to files", {
  skip_on_cran()
  plan <- drake_plan(
    target1 = {
      file_in("in1.txt", "in2.txt")
      file_out("out1.txt", "out2.txt")
      file.create("out1.txt")
      file.create("out2.txt")
    },
    target2 = {
      file_in("out1.txt", "out2.txt")
      file_out("out3.txt", "out4.txt")
      file.create("out3.txt")
      file.create("out4.txt")
      target1
    }
  )
  writeLines("in1", "in1.txt")
  writeLines("in2", "in2.txt")
  cache <- storr::storr_environment()
  make(
    plan,
    cache = cache,
    session_info = FALSE
  )
  config <- drake_config(
    plan,
    cache = cache,
    session_info = FALSE
  )
  writeLines("abcdefg", "out3.txt")
  expect_equal(outdated(config), "target2")
  skip_if_not_installed("lubridate")
  skip_if_not_installed("visNetwork")
  info <- drake_graph_info(
    config,
    show_output_files = TRUE,
    targets_only = TRUE
  )
  expect_equal(
    sort(info$nodes$id),
    sort(c(plan$target, file_store(paste0("out", 1:4, ".txt"))))
  )
  for (file in paste0("out", 1:2, ".txt")) {
    expect_equal(
      info$nodes[info$nodes$id == file_store(file), ]$status,
      "up to date"
    )
  }
  for (file in paste0("out", 3:4, ".txt")) {
    expect_equal(
      info$nodes[info$nodes$id == file_store(file), ]$status,
      "outdated"
    )
  }
  e <- info$edges[with(info$edges, order(from, to)), ]
  expect_equal(
    sort(e$from),
    sort(c(
      file_store(paste0("out", 1:2, ".txt")),
      paste0("target", c(1, rep(1:2, each = 2)))
    ))
  )
  expect_equal(
    sort(e$to),
    sort(c(
      rep("target2", 2),
      file_store(paste0("out", 1:2, ".txt")),
      "target2",
      file_store(paste0("out", 3:4, ".txt"))
    ))
  )
  info <- drake_graph_info(
    config,
    show_output_files = FALSE,
    targets_only = TRUE
  )
  expect_equal(sort(info$nodes$id), sort(paste0("target", 1:2)))
  expect_equal(info$edges$from, "target1")
  expect_equal(info$edges$to, "target2")
    info <- drake_graph_info(
    config,
    show_output_files = TRUE,
    targets_only = TRUE,
    group = "status",
    clusters = "up to date"
  )
  expect_equal(
    sort(info$nodes$id),
    sort(c(file_store(
      paste0("out", 3:4, ".txt")), "status: up to date", "target2"))
  )
})
