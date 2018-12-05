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
  expect_identical(list(), drake_pmap(list(), sum, na.rm = TRUE))
  
  # Catches unequally-lengthed sublists
  x[[2]] <- NULL
  expect_error(drake_pmap(list(x, y, z), sum))
})
