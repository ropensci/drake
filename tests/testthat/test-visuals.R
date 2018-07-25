drake_context("visuals")

test_with_dir("visNetwork graph runs", {
  skip_on_cran()
  skip_if_not_installed("visNetwork")
  config <- dbug()
  pdf(NULL)
  tmp <- vis_drake_graph(config)
  dev.off()
  pdf(NULL)
  tmp <- vis_drake_graph(config, full_legend = FALSE)
  dev.off()
  unlink("Rplots.pdf", force = TRUE)
  file <- "graph.html"
  expect_false(file.exists(file))
  vis_drake_graph(config = config, file = file, selfcontained = TRUE)
  expect_true(file.exists(file))
  unlink(file, force = TRUE, recursive = TRUE)
  skip_if_not_installed("webshot")
  file <- "graph.png"
  expect_false(file.exists(file))
  vis_drake_graph(config = config, file = file, selfcontained = FALSE)
  expect_true(file.exists(file))
  expect_false(any(grepl("*.html", list.files())))
  expect_false(any(grepl("*_files", list.files())))
})

test_with_dir("visNetwork dep graph does not fail if input file is binary", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("visNetwork")
  x <- drake_plan(
    y = readRDS(file_in("input.rds")),
    strings_in_dots = "literals"
  )
  saveRDS(as.list(mtcars), "input.rds")
  con <- drake_config(x, verbose = FALSE)
  expect_silent(out <- vis_drake_graph(con))
  unlink("input.rds", force = TRUE)
})

test_with_dir("ggraphs", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("ggraph")
  load_mtcars_example()
  config <- drake_config(
    my_plan, cache = storr::storr_environment(), session_info = FALSE)
  gg <- drake_ggraph(config)
  expect_true(inherits(gg, "ggplot"))
  make(config = config)
  gg <- drake_ggraph(config)
  expect_true(inherits(gg, "ggplot"))
  if ("package:ggraph" %in% search()){
    eval(parse(text = "detach('package:ggraph', unload = TRUE)"))
  }
})

test_with_dir("Sankey diagram runs", {
  skip_on_cran()
  skip_if_not_installed("networkD3")
  config <- dbug()
  pdf(NULL)
  tmp <- sankey_drake_graph(config)
  dev.off()
  unlink("Rplots.pdf", force = TRUE)
  file <- "graph.html"
  expect_false(file.exists(file))
  sankey_drake_graph(config = config, file = file)
  expect_true(file.exists(file))
  skip_if_not_installed("webshot")
  unlink(file)
  unlink("*_files", recursive = TRUE)
  file <- "graph.png"
  expect_false(file.exists(file))
  sankey_drake_graph(config = config, file = file, selfcontained = FALSE)
  expect_true(file.exists(file))
  expect_false(any(grepl("*.html", list.files())))
  expect_false(any(grepl("*_files", list.files())))
})
