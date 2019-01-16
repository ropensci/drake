drake_context("utils")

test_with_dir("file system", {
  expect_equal(file_extn("a.b/c.d/e/f/g_h.i.j.k"), "k")
  expect_equal(file_extn("123"), "123")
})

test_with_dir("drake_pmap", {
  # Basic functionality: example from purrr::pmap
  x <- list(1, 10, 100)
  y <- list(1, 2, 3)
  z <- list(5, 50, 500)
  ans <- list(x[[1]] + y[[1]] + z[[1]],
              x[[2]] + y[[2]] + z[[2]],
              x[[3]] + y[[3]] + z[[3]])
  expect_identical(ans, drake_pmap(list(x, y, z), sum))

  # Catches inputs of wrong type
  expect_error(drake_pmap("not a list", sum))
  expect_error(drake_pmap(list(), "not a function"))

  # Handles empty list
  expect_identical(list(), drake_pmap(list(), sum))

  # Passes dots to function
  x[2] <- NA
  ans[[2]] <- sum(x[[2]], y[[2]], z[[2]], na.rm = TRUE)
  expect_identical(ans, drake_pmap(list(x, y, z), sum, na.rm = TRUE))

  # Catches unequally-lengthed sublists
  x[[2]] <- NULL
  expect_error(drake_pmap(list(x, y, z), sum))
})

test_with_dir("operators", {
  expect_equal("a" %||% "b", "a")
  expect_equal(NULL %||% "b", "b")
  expect_true(is.numeric(Inf %||% "b"))
  expect_true(is.na(NA %||% "b"))
  expect_equal("a" %||NA% "b", "a")
  expect_equal(NULL %||NA% "b", "b")
  expect_true(is.numeric(Inf %||NA% "b"))
  expect_false(is.na(NA %||NA% "b"))
})

test_with_dir("unlock_environment()", {
  expect_error(
    unlock_environment(NULL),
    regexp = "use of NULL environment is defunct"
  )
  expect_error(
    unlock_environment("x"),
    regexp = "not an environment"
  )
  e <- new.env(parent = emptyenv())
  e$y <- 1
  expect_false(environmentIsLocked(e))
  assign(x = ".x", value = "x", envir = e)
  expect_equal(get(x = ".x", envir = e), "x")
  lock_environment(e)
  msg1 <- "cannot change value of locked binding"
  msg2 <- "cannot add bindings to a locked environment"
  expect_true(environmentIsLocked(e))
  assign(x = ".x", value = "y", envir = e)
  expect_equal(get(x = ".x", envir = e), "y")
  expect_error(assign(x = "y", value = "y", envir = e), regexp = msg1)
  expect_error(assign(x = "a", value = "x", envir = e), regexp = msg2)
  expect_error(assign(x = "b", value = "y", envir = e), regexp = msg2)
  unlock_environment(e)
  assign(x = ".x", value = "1", envir = e)
  assign(x = "y", value = "2", envir = e)
  assign(x = "a", value = "x", envir = e)
  expect_equal(get(x = ".x", envir = e), "1")
  expect_equal(get(x = "y", envir = e), "2")
  expect_equal(get(x = "a", envir = e), "x")
  expect_false(environmentIsLocked(e))
  unlock_environment(e)
  assign(x = "b", value = "y", envir = e)
  expect_equal(get(x = "b", envir = e), "y")
  expect_false(environmentIsLocked(e))
})

test_with_dir("weak_tibble", {
  skip_on_cran()

  for (fdf in c(FALSE, TRUE)) {
    out <- weak_tibble(.force_df = fdf)
    expect_equivalent(out, data.frame())
    expect_equivalent(weak_as_tibble(list(), .force_df = fdf), data.frame())
  }

  # No factors
  out <- weak_tibble(a = 1:2, b = c("x", "y"), .force_df = TRUE)
  exp <- data.frame(a = 1:2, b = c("x", "y"), stringsAsFactors = FALSE)
  expect_equivalent(out, exp)
  out <- weak_as_tibble(list(a = 1:2, b = c("x", "y")))
  expect_equivalent(out, exp)
})

test_with_dir("encoding empty keys", {
  x <- character(0)
  expect_equal(encode_path(x), x)
  expect_equal(decode_path(x), x)
  expect_equal(encode_namespaced(x), x)
  expect_equal(decode_namespaced(x), x)
})

test_with_dir("key encoding for paths and namespaced functions", {
  x <- c("myfunny:::variablename", "relative/path\na\\m//e")
  expect_false(all(is_encoded_path(x)))
  expect_false(all(is_encoded_namespaced(x)))

  y <- encode_path(x)
  z <- encode_namespaced(x)
  expect_false(any(y == z))

  expect_true(all(is_encoded_path(y)))
  expect_false(all(is_encoded_path(z)))

  expect_false(all(is_encoded_namespaced(y)))
  expect_true(all(is_encoded_namespaced(z)))

  expect_equal(decode_path(y), x)
  expect_equal(decode_namespaced(z), x)

  expect_true(all(file.create(y)))
  expect_true(all(file.create(z)))
})

test_with_dir("same with memoization", {
  config <- drake_config(
    drake_plan(targ = 1),
    cache = storr::storr_environment(),
    session_info = FALSE
  )
  x <- c("myfunny:::variablename", "relative/path\na\\m//e")
  expect_false(all(is_encoded_path(x)))
  expect_false(all(is_encoded_namespaced(x)))
  for (i in 1:3) {
    y <- encode_path(x, config = config)
    z <- encode_namespaced(x, config = config)
    expect_false(any(y == z))  i <- 1

    expect_true(all(is_encoded_path(y)))
    expect_false(all(is_encoded_path(z)))

    expect_false(all(is_encoded_namespaced(y)))
    expect_true(all(is_encoded_namespaced(z)))

    expect_equal(decode_path(y, config = config), x)
    expect_equal(decode_namespaced(z, config = config), x)

    expect_true(all(file.create(y)))
    expect_true(all(file.create(z)))
  }
})

test_with_dir("safe_get", {
  config <- drake_config(drake_plan(a = 1))
  expect_true(is.na(safe_get("a", "b", config)))
  expect_true(is.na(safe_get_hash("a", "b", config)))
})

test_with_dir("complete_cases()", {
  for (empty in list(data.frame(), mtcars[NULL, ], mtcars[, NULL])) {
    expect_equivalent(complete_cases(empty), logical(0))
  }
  x <- data.frame(a = letters, b = LETTERS)
  expect_equal(complete_cases(x), rep(TRUE, length(letters)))
  x <- data.frame(a = 1:6, b = c(1:3, rep(NA_integer_, 3)))
  expect_equal(complete_cases(x), rep(c(TRUE, FALSE), each = 3))
})
