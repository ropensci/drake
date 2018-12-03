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
