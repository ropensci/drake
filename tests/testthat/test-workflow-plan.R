drake_context("workflow plan")

test_with_dir("duplicated targets", {
  expect_error(
    drake_plan(
      a = 1,
      a = 2,
      b = 1,
      b = 2,
      c = 3
    ),
    regexp = "Duplicated targets"
  )
  expect_error(
    bind_plans(
      drake_plan(a = 1, b = 1, c = 1),
      drake_plan(a = 5, b = 2, d = 5)
    ),
    regexp = "Duplicated targets"
  )
  expect_equal(
    bind_plans(
      drake_plan(a = 1, b = 1, c = 1),
      drake_plan(a = 1, b = 1, d = 5)
    ),
    drake_plan(
      a = 1,
      b = 1,
      c = 1,
      d = 5
    )
  )
  expect_equal(
    bind_plans(
      drake_plan(d = f(c, b)),
      drake_plan(c = f(a), a = 5),
      drake_plan(b = f(a), a = 5)
    ),
    drake_plan(
      d = f(c, b),
      c = f(a),
      a = 5,
      b = f(a)
    )
  )
})

test_with_dir("warn about <- and -> in drake_plan()", {
  expect_silent(tmp <- drake_plan())
  expect_silent(tmp <- drake_plan(a = 1, b = 2))
  expect_silent(
    tmp <- drake_plan(
      a = {
        x <- 1
        x
      }
    )
  )
  expect_silent(
    tmp <- drake_plan(
      a = x <- 1,
      b = 2
    )
  )
  expect_silent(
    tmp <- drake_plan(
      a = 1 -> x,
      b = 2
    )
  )
  expect_warning(
    tmp <- drake_plan(a = 1, b <- 2),
    regexp = "to assign targets to commands"
  )
  expect_warning(
    tmp <- drake_plan(a = 1, b -> 2),
    regexp = "to assign targets to commands"
  )
  expect_warning(
    tmp <- drake_plan(a <- 1, b -> 2),
    regexp = "to assign targets to commands"
  )
})

test_with_dir("File functions handle input", {
  expect_equal(
    file_in(1, "x", "y"), c("1", "x", "y")
  )
  expect_equal(
    knitr_in(1, "x", "y"), c("1", "x", "y")
  )
  expect_warning(expect_equal(file_out(c(1, "x", "y")), "1"))
  expect_error(file_out(1, "x", "y"))
  expect_equal(
    code_dependencies(quote(file_out(c("file1", "file2")))),
    list(file_out = drake_quotes(c("file1", "file2"), single = FALSE))
  )
  expect_error(
    single_file_out(""),
    regexp = "found an empty"
  )
})

test_with_dir("edge cases for plans", {
  # empty plan
  expect_equal(
    drake_plan(),
    tibble(
      target = character(0),
      command = character(0)
    )
  )
  # no target names
  expect_equal(
    drake_plan(a, b),
    tibble(
      target = c("drake_target_1", "drake_target_2"),
      command = c("a", "b")
    )
  )
  expect_equal(
    drake_plan(list = c("a", "b")),
    drake_plan(a, b)
  )
  # incomplete target names
  expect_equal(
    drake_plan(a = 1, b),
    tibble(
      target = c("a", "drake_target_1"),
      command = c("1", "b")
    )
  )
  # too many file outputs
  expect_warning(expect_equal(
    drake_plan(a = file_out("file1", "file2")),
    tibble(
      target = c("\"file1\""),
      command = "file_out('file1', 'file2')"
    )
  ))
  expect_warning(expect_equal(
    drake_plan(a = file_out(c("file1", "file2"))),
    tibble(
      target = c("\"file1\""),
      command = "file_out(c('file1', 'file2'))"
    )
  ))
})

test_with_dir("plan set 1", {
  for (tidy_evaluation in c(TRUE, FALSE)){
    expect_warning(x <- drake_plan(
      a = c,
      b = "c",
      list = c(c = "d", d = "readRDS('e')"),
      tidy_evaluation = tidy_evaluation,
      strings_in_dots = "filenames"
    ))
    y <- tibble(
      target = letters[1:4],
      command = c("c", "'c'",
      "d", "readRDS('e')"))
    expect_equal(x, y)
    expect_warning(check_plan(x))
  }
})

test_with_dir("plan set 2", {
  for (tidy_evaluation in c(TRUE, FALSE)){
    x <- drake_plan(
      a = c,
      b = "c",
      list = c(c = "d", d = "readRDS('e')"),
      strings_in_dots = "literals",
      tidy_evaluation = tidy_evaluation
    )
    y <- tibble(
      target = letters[1:4],
      command = c("c", "\"c\"",
                  "d", "readRDS('e')"))
    expect_equal(x, y)
  }
})

test_with_dir("plan set 3", {
  for (tidy_evaluation in c(TRUE, FALSE)){
  expect_warning(x <- drake_plan(
    a = c,
    b = "c",
    list = c(c = "d", d = "readRDS('e')"),
    strings_in_dots = "literals", file_targets = TRUE,
    tidy_evaluation = tidy_evaluation))
  y <- tibble::tibble(
    target = drake::drake_quotes(letters[1:4], single = FALSE),
    command = c("c", "\"c\"", "d", "readRDS('e')"))
  expect_equal(x, y)
  }
})

test_with_dir("drake_plan() trims outer whitespace in target names", {
  for (tidy_evaluation in c(TRUE, FALSE)){
    x <- drake_plan(list = c(` a` = 1, `b \t\n` = 2),
                    tidy_evaluation = tidy_evaluation)
    y <- drake_plan(a = 1, b = 2, tidy_evaluation = tidy_evaluation)
    expect_equal(x$target, y$target)
  }
})

test_with_dir("make() and check_plan() trim outer whitespace in target names", {
  x <- tibble(target = c("a\n", "  b", "c ", "\t  d   "),
                  command = 1)
  expect_silent(make(x, verbose = FALSE, session_info = FALSE))
  expect_equal(sort(cached()), letters[1:4])
  stat <- c(a = "finished", b = "finished", c = "finished",
            d = "finished")
  expect_equal(progress(), stat)

  expect_warning(
    con <- make(
      x,
      verbose = FALSE,
      targets = c("a", "nobody_home"),
      session_info = FALSE
    )
  )
  expect_true(all(letters[1:4] %in% cached()))
  expect_true(all(letters[1:4] %in% con$plan$target))
})

test_with_dir("make() plays nicely with tibbles", {
  skip_if_not_installed("pillar")
  skip_if_not_installed("tibble")
  x <- tibble::tribble(~target, ~command, "nothing", 1)
  expect_silent(check_plan(x, verbose = FALSE))
  expect_silent(make(x, verbose = FALSE, session_info = FALSE))
})

test_with_dir("check_plan() finds bad symbols", {
  x <- tibble(
    target = c("gotcha", "b", "\"targs\"", "a'x'", "b'x'"),
    command = 1)
  expect_warning(o <- check_plan(x, verbose = FALSE))
  x <- tibble(
    target = c("\"targs\""),
    command = 1)
  expect_silent(o <- check_plan(x, verbose = FALSE))
  x <- tibble(
    target = c("gotcha", "b", "targs"),
    command = 1)
  expect_silent(o <- check_plan(x, verbose = FALSE))
})

test_with_dir("illegal target names get fixed", {
  pl <- tibble(
    target = c("_a", "a^", "a*", "a-"),
    command = 1
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
  out2 <- tibble(
    target = c("test_1_4", "test_5_8", "test_9_12"),
    command = c("run_it(1:4)", "run_it(5:8)", "run_it(9:12)")
  )
  expect_equal(out, out2)
})

test_with_dir("file names with weird characters do not get mangled", {
  out <- tibble(
    target = c("\"is:a:file\"", "not:a:file"),
    command = as.character(1:2)
  )
  out2 <- expect_warning(sanitize_plan(out))
  out3 <- tibble(
    target = c("\"is:a:file\"", "not_a_file"),
    command = as.character(1:2)
  )
  expect_equal(out[1, ], out2[1, ])
  expect_false(identical(out[2, ], out2[2, ]))
  expect_equal(out2, out3)
})

test_with_dir("can use semicolons for multi-line commands", {
  plan <- drake_plan(list = c(x = "a<-1; a", y = "b<-2\nb"))
  make(plan, verbose = FALSE, session_info = FALSE)
  expect_false(any(c("a", "b") %in% ls()))
  expect_true(all(cached(x, y, search = FALSE)))
  expect_equal(cached(search = FALSE), c("x", "y"))
})

test_with_dir("can use braces for multi-line commands", {
  small_plan <- drake_plan(
    small_target = {
      local_object <- 1 + 1
      2 + local_object
    }
  )
  make(small_plan, session_info = FALSE)
  expect_true("small_target" %in% cached())
  expect_false("local_object_target" %in% cached())
  expect_equal(readd(small_target), 4)
  expect_false("local_object" %in% ls())
})

test_with_dir("ignore() suppresses updates", {
  cache <- storr::storr_environment()
  envir <- new.env(parent = globalenv())
  envir$arg <- 4

  # Without ignore()
  con <- make(
    plan = drake_plan(x = sqrt(arg)),
    envir = envir,
    cache = cache
  )
  expect_equal(justbuilt(con), "x")
  con$envir$arg <- con$envir$arg + 1
  con <- make_with_config(con)
  expect_equal(justbuilt(con), "x")

  # With ignore()
  con <- make(
    plan = drake_plan(x = sqrt( ignore(arg) + 123)),
    envir = envir,
    cache = cache
  )
  expect_equal(justbuilt(con), "x")
  con$envir$arg <- con$envir$arg + 1
  con <- make_with_config(con)
  expect_equal(justbuilt(con), character(0))

  con$envir$arg2 <- con$envir$arg + 1234
  con$plan <- drake_plan(x = sqrt( ignore  (arg2 ) + 123))
  con <- make_with_config(con)
  expect_equal(justbuilt(con), character(0))
})

test_with_dir("ignore() works on its own", {
  expect_equal(ignore(), NULL)
  expect_equal(ignore(1234), 1234)
  expect_identical(ignore_ignore(digest::digest), digest::digest)
})

test_with_dir("standardized commands with ignore()", {
  expect_equal(standardize_command("sqrt(arg)"), "{\n sqrt(arg) \n}")
  expect_equal(
    standardize_command("f(sqrt( ignore(fun(arg) + 7) + 123))"),
    "{\n f(sqrt(ignore() + 123)) \n}"
  )
  expect_equal(
    standardize_command("f(sqrt( ignore  (fun(arg) + 7) + 123) )"),
    "{\n f(sqrt(ignore() + 123)) \n}"
  )
  expect_equal(
    standardize_command(" f (sqrt( drake::ignore(fun(arg) + 7) + 123 ))"),
    "{\n f(sqrt(ignore() + 123)) \n}"
  )
  expect_equal(
    standardize_command("\tf(sqrt( drake ::: ignore  (fun(arg) + 7) + 123))"),
    "{\n f(sqrt(ignore() + 123)) \n}"
  )
  expect_equal(
    standardize_command("function(x){(sqrt( ignore(fun(arg) + 7) + 123))}"),
    "{\n function(x) {\n    (sqrt(ignore() + 123))\n} \n}"
  )
  f <- function(x){
    (sqrt( ignore(fun(arg) + 7) + 123))
  }
  b <- body(ignore_ignore(f))
  for (a in names(attributes(b))){
    attr(b, a) <- NULL
  }
  expect_equal(b, quote({  (sqrt(ignore() + 123)) })) # nolint
})

test_with_dir("ignore() in imported functions", {
  f <- function(x){
    (sqrt( ignore(sqrt(x) + 7) + 123))
  }
  plan <- drake_plan(x = f(1))
  cache <- storr::storr_environment()
  config <- make(plan, cache = cache)
  expect_equal(justbuilt(config), "x")
  expect_equal(readd(f, cache = cache), f)
  expect_equal(
    readd(f, cache = cache, namespace = "kernels")[3],
    "    (sqrt(ignore() + 123))"
  )
  f <- function(x){
    (sqrt( ignore(sqrt(x) + 8) + 123))
  }
  config <- make(plan, cache = cache)
  expect_equal(justbuilt(config), character(0))
  f <- function(x){
    (sqrt( ignore(sqrt(x) + 8) + 124))
  }
  config <- make(plan, cache = cache)
  expect_equal(justbuilt(config), "x")
})

test_with_dir("custom column interface", {
  plan <- drake_plan(
    x = target(
      command = 1 + 2,
      trigger = "always",
      user_column_1 = 1,
      user_column_2 = "some text"
    ),
    y = target(
      command = Sys.sleep("not a number"),
      col3 = "some text"
    ),
    z = rnorm(10),
    strings_in_dots = "literals"
  )
  plan0 <- tibble::tibble(
    target = c("x", "y", "z"),
    command = c("1 + 2", "Sys.sleep(\"not a number\")", "rnorm(10)"),
    trigger = c("always", "any", "any"),
    user_column_1 = c(1, NA, NA),
    user_column_2 = c("some text", NA, NA),
    col3 = c(NA, "some text", NA)
  )
  cn <- colnames(plan)
  expect_equal(cn, colnames(plan0))
  expect_equal(plan[, cn], plan0[, cn])
})

test_with_dir("bind_plans()", {
  plan1 <- drake_plan(x = 1, y = 2)
  plan2 <- drake_plan(
    z = target(
      command = download_data(),
      trigger = "always"
    ),
    strings_in_dots = "literals"
  )
  plan3 <- bind_plans(plan1, plan2)
  plan4 <- tibble::tibble(
    target = c("x", "y", "z"),
    command = c("1", "2", "download_data()"),
    trigger = c("any", "any", "always")
  )
  expect_equal(plan3, plan4)
})
