context("other-features")

test_with_dir("in_progress() works", {
  expect_equal(in_progress(), character(0))
  bad_plan <- plan(x = function_doesnt_exist())
  expect_error(make(bad_plan, verbose = FALSE))
  expect_equal(in_progress(), "x")
})

test_with_dir("missed() works", {
  o <- dbug()
  expect_equal(character(0), missed(o$plan, envir = o$envir,
    verbose = F))
  rm(list = c("f", "g"), envir = o$envir)
  expect_equal(sort(c("f", "g")), sort(missed(o$plan, envir = o$envir,
    verbose = F)))
})

test_with_dir(".onLoad() warns correctly and .onAttach() works", {
  f <- ".RData"
  expect_false(file.exists(f))
  expect_silent(drake:::.onLoad())
  save.image()
  expect_true(file.exists(f))
  expect_warning(drake:::.onLoad())
  unlink(f, force = TRUE)
  set.seed(0)
  expect_true(is.character(drake_tip()))
  expect_silent(suppressPackageStartupMessages(drake:::.onAttach()))
})

test_with_dir("graph functions work", {
  config <- dbug()
  expect_equal(class(build_graph(config$plan)), "igraph")
  pdf(NULL)
  tmp <- plot_graph(plan = config$plan, envir = config$envir,
    verbose = FALSE)
  dev.off()
  unlink("Rplots.pdf", force = TRUE)
  expect_true(is.character(default_graph_title(
    parallelism = parallelism_choices()[1], split_columns = FALSE)))
  expect_true(is.character(default_graph_title(
    parallelism = parallelism_choices()[1], split_columns = TRUE)))
})

test_with_dir("check_config() via check() and make()", {
  config <- dbug()
  y <- data.frame(x = 1, y = 2)
  expect_error(check(y, envir = config$envir))
  expect_error(make(y, envir = config$envir))
  y <- data.frame(target = character(0), command = character(0))
  expect_error(check(y, envir = config$envir))
  expect_error(make(y, envir = config$envir))
  expect_error(
    check(config$plan, targets = character(0), envir = config$envir))
  expect_error(
    make(config$plan, targets = character(0), envir = config$envir))
})

test_with_dir("deprecation", {
  plan <- data.frame(code = 1, output = "x")
  expect_warning(make(plan, verbose = FALSE))

  expect_warning(make(plan, verbose = FALSE))
  expect_warning(status())
  expect_true(is.numeric(readd(x, search = FALSE)))
})

test_with_dir("targets can be partially specified", {
  config <- dbug()
  config$targets <- "'intermediatefile.rds'"
  testrun(config)
  expect_true(file.exists("intermediatefile.rds"))
  expect_error(readd(final, search = FALSE))
  config$targets <- "final"
  testrun(config)
  expect_true(is.numeric(readd(final, search = FALSE)))
})

test_with_dir("misc stuff", {
  expect_equal(as_file("x"), "'x'")
})

test_with_dir("misc empty/NULL cases", {
  clean(list = "no_cache")
})

test_with_dir("unique_random_string", {
  set.seed(2017)
  x <- unique_random_string(n = 15)
  y <- unique_random_string(exclude = "a", n = 10)
  expect_equal(nchar(x), 15)
  expect_equal(nchar(y), 10)
  exclude <- c(letters, LETTERS, 1:9)
  for (i in 1:100){
    expect_equal(
      unique_random_string(exclude = exclude, n = 1),
      "0"
    )
  }
})
