drake_context("workflow plan")

test_with_dir("empty plan", {
  expect_equal(
    drake_plan(),
    data.frame(
      target = character(0),
      command = character(0)
    )
  )
})

test_with_dir("plan set 1", {
  x <- drake_plan(
    a = c,
    b = "c",
    list = c(c = "d", d = "readRDS('e')"))
  y <- data.frame(
    target = letters[1:4],
    command = c("c", "'c'",
    "d", "readRDS('e')"),
    stringsAsFactors = F)
  expect_equal(x, y)
})

test_with_dir("plan set 2", {
  x <- drake_plan(a = c,
    b = "c",
    list = c(c = "d", d = "readRDS('e')"),
    strings_in_dots = "literals")
  y <- data.frame(
    target = letters[1:4],
    command = c("c", "\"c\"",
    "d", "readRDS('e')"), stringsAsFactors = F)
  expect_equal(x, y)
})

test_with_dir("plan set 3", {
  x <- drake_plan(
    a = c,
    b = "c",
    list = c(c = "d", d = "readRDS('e')"),
    strings_in_dots = "literals", file_targets = TRUE)
  y <- data.frame(
    target = drake::drake_quotes(letters[1:4], single = TRUE),
    command = c("c", "\"c\"", "d", "readRDS('e')"),
    stringsAsFactors = F)
  expect_equal(x, y)
})

test_with_dir("plan set 4", {
  x <- drake_plan(
    a = c,
    b = "c",
    list = c(c = "d", d = "readRDS('e')"),
    strings_in_dots = "filenames", file_targets = TRUE)
  y <- data.frame(
    target = drake::drake_quotes(letters[1:4], single = TRUE),
    command = c("c", "'c'", "d", "readRDS('e')"), stringsAsFactors = F)
  expect_equal(x, y)
  expect_warning(check_plan(x, verbose = FALSE))
})

test_with_dir("drake_plan() trims outer whitespace in target names", {
  x <- drake_plan(list = c(` a` = 1, `b \t\n` = 2))
  y <- drake_plan(a = 1, b = 2)
  expect_equal(x$target, y$target)
})

test_with_dir("make() and check_plan() trim outer whitespace in target names", {
  x <- data.frame(target = c("a\n", "  b", "c ", "\t  d   "),
    command = 1)
  expect_silent(make(x, verbose = FALSE, session_info = FALSE))
  expect_equal(sort(cached()), letters[1:4])
  stat <- c(a = "finished", b = "finished", c = "finished",
    d = "finished")
  expect_equal(progress(), stat)

  expect_warning(make(x, verbose = FALSE, targets = c("a",
    "nobody_home"), session_info = FALSE))

  x <- data.frame(target = c("a", " a"), command = 1)
  expect_error(check_plan(x, verbose = FALSE))
})

test_with_dir("make() plays nicely with tibbles", {
  if (!("tibble" %in% rownames(installed.packages()))){
    skip("Package tibble not installed.")
  }
  x <- tibble::tribble(~target, ~command, "nothing", 1)
  expect_silent(check_plan(x, verbose = FALSE))
  expect_silent(make(x, verbose = FALSE, session_info = FALSE))
})

test_with_dir("check_plan() finds bad symbols", {
  x <- data.frame(
    target = c("gotcha", "b", "\"targs\"", "a'x'", "b'x'"),
    command = 1)
  expect_warning(o <- check_plan(x, verbose = FALSE))
  x <- data.frame(
    target = c("gotcha", "b", "\"targs\""),
    command = 1)
  expect_silent(o <- check_plan(x, verbose = FALSE))
})

test_with_dir("illegal target names get fixed", {
  pl <- data.frame(
    target = c("_a", "a^", "a*", "a-"),
    command = 1,
    stringsAsFactors = FALSE
  )
  cache <- storr::storr_environment()
  expect_warning(
    con <- make(pl, cache = cache, session_info = FALSE)
  )
  expect_equal(
    sort(con$plan$target),
    sort(con$targets),
    sort(cached(cache = cache)),
    sort(c("a", "a_", "a__1", "a__2"))
  )
})

test_with_dir("issue 187 on Github (from Kendon Bell)", {
  test <- drake_plan(test = run_it(wc__))
  out <- expect_warning(
    evaluate_plan(test, rules = list(wc__ = list(1:4, 5:8, 9:12)))
  )
  out2 <- data.frame(
    target = c("test_1_4", "test_5_8", "test_9_12"),
    command = c("run_it(1:4)", "run_it(5:8)", "run_it(9:12)"),
    stringsAsFactors = FALSE
  )
  expect_equal(out, out2)
})

test_with_dir("file names with weird characters do not get mangled", {
  out <- data.frame(
    target = c("'is:a:file'", "not:a:file"),
    command = as.character(1:2),
    stringsAsFactors = FALSE
  )
  out2 <- expect_warning(sanitize_plan(out))
  out3 <- data.frame(
    target = c("'is:a:file'", "not_a_file"),
    command = as.character(1:2),
    stringsAsFactors = FALSE
  )
  expect_equal(out2, out3)
})
