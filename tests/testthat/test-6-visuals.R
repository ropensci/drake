drake_context("visuals")

test_with_dir("file system", {
  expect_equal(file_extn("a.b/c.d/e/f/g_h.i.j.k"), "k")
  expect_equal(file_extn("123"), "123")
})

test_with_dir("visNetwork graph runs", {
  skip_on_cran()
  skip_if_not_installed("lubridate")
  skip_if_not_installed("visNetwork")
  config <- dbug()
  pdf(NULL)
  graph <- plot(dbug_plan())
  expect_true(inherits(graph, "visNetwork"))
  tmp <- vis_drake_graph_impl(config)
  dev.off()
  for (hover in c(TRUE, FALSE)) {
    pdf(NULL)
    tmp <- vis_drake_graph_impl(config, full_legend = FALSE, hover = hover)
    dev.off()
  }
  unlink("Rplots.pdf", force = TRUE)
  file <- "graph.html"
  expect_false(file.exists(file))
  vis_drake_graph_impl(config = config, file = file, selfcontained = FALSE)
  expect_true(file.exists(file))
  unlink(file, force = TRUE, recursive = TRUE)
  unlink("*_files", force = TRUE, recursive = TRUE)
  skip_on_appveyor()
  skip_if_not_installed("webshot")
  skip_if(!webshot::is_phantomjs_installed())
  file <- "graph.png"
  expect_false(file.exists(file))
  vis_drake_graph_impl(config = config, file = file, selfcontained = FALSE)
  expect_true(file.exists(file))
  expect_false(any(grepl("*.html", list.files())))
  expect_false(any(grepl("*_files", list.files())))
})

test_with_dir("visNetwork dep graph does not fail if input file is binary", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("datasets")
  skip_if_not_installed("lubridate")
  skip_if_not_installed("visNetwork")
  x <- drake_plan(y = readRDS(file_in("input.rds")))
  saveRDS(as.list(datasets::mtcars), "input.rds")
  con <- drake_config(x, verbose = 0L)
  expect_silent(out <- vis_drake_graph_impl(con))
  unlink("input.rds", force = TRUE)
})

test_with_dir("ggraphs", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggraph")
  skip_if_not_installed("lubridate")
  skip_if_not_installed("visNetwork")
  load_mtcars_example()
  config <- drake_config(
    my_plan, cache = storr::storr_environment(), session_info = FALSE)
  gg <- drake_ggraph_impl(config, label_nodes = FALSE)
  gg <- drake_ggraph_impl(config, label_nodes = TRUE)
  expect_true(inherits(gg, "ggplot"))
  make_impl(config = config)
  gg <- drake_ggraph_impl(config)
  expect_true(inherits(gg, "ggplot"))
  if ("package:ggraph" %in% search()) {
    suppressWarnings(detach("package:ggraph", unload = TRUE)) # nolint
  }
})

test_with_dir("Sankey diagram runs", {
  skip_on_cran()
  skip_if_not_installed("lubridate")
  skip_if_not_installed("networkD3")
  skip_if_not_installed("visNetwork")
  config <- dbug()
  pdf(NULL)
  tmp <- sankey_drake_graph_impl(config)
  dev.off()
  unlink("Rplots.pdf", force = TRUE)
  file <- "graph.html"
  expect_false(file.exists(file))
  sankey_drake_graph_impl(config = config, file = file, selfcontained = FALSE)
  expect_true(file.exists(file))
  skip_on_appveyor()
  skip_if_not_installed("webshot")
  skip_if(!webshot::is_phantomjs_installed())
  unlink(file, force = TRUE, recursive = TRUE)
  unlink("*_files", force = TRUE, recursive = TRUE)
  file <- "graph.png"
  expect_false(file.exists(file))
  sankey_drake_graph_impl(config = config, file = file, selfcontained = FALSE)
  expect_true(file.exists(file))
  expect_false(any(grepl("*.html", list.files())))
  expect_false(any(grepl("*_files", list.files())))
})

test_with_dir("colors and shapes", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_is(node_color("target"), "character")
  expect_is(node_color("import"), "character")
  expect_is(node_color("not found"), "character")
  expect_is(node_color("not found"), "character")
  expect_equal(node_color("bluhlaksjdf"), node_color("other"))
  expect_is(node_shape("object"), "character")
  expect_is(node_shape("file"), "character")
  expect_is(node_shape("not found"), "character")
  expect_equal(node_shape("bluhlaksjdf"), node_shape("other"))
})

test_with_dir("shapes", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_is(node_shape("target"), "character")
  expect_is(node_shape("import"), "character")
  expect_is(node_shape("not found"), "character")
  expect_is(node_shape("object"), "character")
  expect_is(node_color("file"), "character")
  expect_is(node_color("not found"), "character")
  expect_equal(node_color("bluhlaksjdf"), node_color("other"))
})

test_with_dir("text graph", {
  skip_on_cran()
  skip_if_not_installed("crayon")
  skip_if_not_installed("txtplot")
  skip_if_not_installed("visNetwork")
  load_mtcars_example()
  config <- drake_config(
    my_plan,
    session_info = FALSE,
    cache = storr::storr_environment()
  )
  expect_message(text_drake_graph_impl(config))
  expect_message(text_drake_graph_impl(config, nchar = 0L))
  expect_message(text_drake_graph_impl(config, nchar = 5L))
})
