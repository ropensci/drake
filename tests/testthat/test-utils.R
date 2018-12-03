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

test_with_dir("parse_dots", {
  expect_equal(parse_dots(), character(0))
  expect_equal(parse_dots(list = c("a", "b")), c("a", "b"))
  expect_equal(parse_dots(a, b), c("a", "b"))
  expect_equal(parse_dots(a, b, "c"), c("a", "b", "c"))
  expect_equal(parse_dots(a, b, list = c("c", "d")), c("a", "b", "c", "d"))
  expect_equal(
    parse_dots(a, b, "c", list = c("d", "e")),
    c("a", "b", "c", "d", "e")
  )
})
