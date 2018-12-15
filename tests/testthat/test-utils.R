drake_context("utils")

test_with_dir("file system", {
  expect_equal(file_extn("a.b/c.d/e/f/g_h.i.j.k"), "k")
  expect_equal(file_extn("123"), "123")
})

test_with_dir("merge_lists()", {
  x <- list(a = 1, b = 1:2, c = 1:3)
  y <- list(b = 3:4, c = 4:5, d = 1:5)
  z <- merge_lists(x, y)
  z <- lapply(z, sort)
  w <- list(a = 1, b = 1:4, c = 1:5, d = 1:5)
  expect_equal(z, w)
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

test_with_dir("weak_tibble", {
  skip_on_cran()

  for(fdf in c(FALSE, TRUE)) {
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
