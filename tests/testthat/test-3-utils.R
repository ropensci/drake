drake_context("utils")

test_with_dir("file_remove()", {
  skip_on_cran()
  expect_silent(file_remove("abc"))
  file.create("abc")
  expect_true(file.exists("abc"))
  expect_silent(file_remove("abc"))
  expect_false(file.exists("abc"))
})

test_with_dir("assert_pkg", {
  skip_on_cran()
  expect_error(assert_pkg("_$$$blabla"), regexp = "not installed")
  expect_error(
    assert_pkg("digest", version = "9999.9999.9999"),
    regexp = "must be version 9999.9999.9999 or greater"
  )
  expect_error(
    assert_pkg(
      "impossible",
      version = "9999.9999.9999",
      install = "installer::install"
    ),
    regexp = "with installer::install"
  )
})

test_with_dir("%||%", {
  expect_equal("a" %||% "b", "a")
  expect_equal(NULL %||% "b", "b")
  expect_equal(character(0) %||% "b", "b")
  expect_true(is.numeric(Inf %||% "b"))
  expect_true(is.na(NA %||% "b"))
})

test_with_dir("%|||%", {
  expect_equal("a" %|||% "b", "a")
  expect_equal(NULL %|||% "b", "b")
  expect_equal(character(0) %|||% "b", character(0))
  expect_true(is.numeric(Inf %|||% "b"))
  expect_true(is.na(NA %|||% "b"))
})

test_with_dir("%||NA%", {
  expect_equal("a" %|||NA% "b", "a")
  expect_equal(NULL %|||NA% "b", NULL)
  expect_true(is.numeric(Inf %|||NA% "b"))
  expect_false(is.na(NA %|||NA% "b"))
})

test_with_dir("%|||NA%", {
  expect_equal("a" %|||NA% "b", "a")
  expect_equal(NULL %|||NA% "b", NULL)
  expect_true(is.numeric(Inf %|||NA% "b"))
  expect_false(is.na(NA %|||NA% "b"))
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

test_with_dir("error handlers", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_equal(error_na(1), NA_character_)
  expect_false(error_false(1))
  expect_error(error_tibble_times(123))
})

test_with_dir("isolate_example()", {
  skip_on_cran()
  isolate_example("example", file.create("abc"))
  expect_false(file.exists("abc"))
})

test_with_dir("text wrapping", {
  skip_on_cran()
  x <- paste(letters, collapse = "")
  expect_equal(x, soft_wrap(x, width = 2))
  expect_equal(x, soft_wrap(x, width = 100))
  expect_equal("ab\ncd\ne", hard_wrap("abcde", width = 2))
  expect_equal(x, hard_wrap(x, width = 100))
})

test_with_dir("dir_move()", {
  dir_create("x")
  dir_create(file.path("x", "y"))
  saveRDS("x", file.path("x", "x"))
  saveRDS("y_a", file.path("x", "y", "y_a"))
  saveRDS("y_b", file.path("x", "y", "y_b"))
  dir_create(file.path("x", "y", "z"))
  saveRDS("z_a", file.path("x", "y", "z", "z_a"))
  saveRDS("z_b", file.path("x", "y", "z", "z_b"))
  file.create(file.path("x", "y", "z", ".gitignore"))
  expect_true(file.exists("x"))
  files_exp <- list.files("x", all.files = TRUE)
  dir_move("x", "tmp")
  expect_false(file.exists("x"))
  expect_true(file.exists("tmp"))
  expect_equal(files_exp, list.files("tmp", all.files = TRUE))
  expect_true(file.exists(file.path("tmp", "y", "z", ".gitignore")))
  expect_equal(readRDS(file.path("tmp", "y", "z", "z_a")), "z_a")
  expect_equal(readRDS(file.path("tmp", "y", "z", "z_b")), "z_b")
  dir.create("tmp2")
  expect_warning(dir_move("tmp", "tmp2"), regexp = "already exists")
  expect_equal(files_exp, list.files("tmp", all.files = TRUE))
  expect_equal(character(0), list.files("tmp2"))
  file.create(file.path("tmp2", "remove_this"))
  dir_move("tmp", "tmp2", overwrite = TRUE)
  expect_equal(files_exp, list.files("tmp2", all.files = TRUE))
  expect_false(file.exists(file.path("tmp2", "remove_this")))
})

test_with_dir("grid_index()", {
  skip_on_cran()
  grid <- expand.grid(seq_len(6), seq_len(3), seq_len(7), seq_len(5))
  grid <- grid[, rev(seq_len(4))]
  size <- unname(vapply(grid, max, FUN.VALUE = integer(1)))
  for (index in seq_len(nrow(grid))) {
    expect_equal(
      as.integer(grid[index,, drop = TRUE]), # nolint
      grid_index(index, size)
    )
  }
})

test_with_dir("custom digest functions give same hashes", {
  out <- digest_murmur32(mtcars, serialize = TRUE)
  exp <- digest::digest(mtcars, algo = "murmur32", serialize = TRUE)
  expect_equal(out, exp)
  object <- digest::digest("abc")
  out <- digest_murmur32(object, serialize = FALSE)
  exp <- digest::digest(object, algo = "murmur32", serialize = FALSE)
  expect_equal(out, exp)
})

test_with_dir("storage_copy() (#1120)", {
  skip_on_cran()
  f1 <- tempfile()
  f2 <- tempfile()
  suppressWarnings(storage_copy(f1, f2))
  expect_false(file.exists(f1))
  expect_false(file.exists(f2))
  file.create(f1)
  storage_copy(f1, f2)
  expect_true(file.exists(f2))
  storage_copy(f1, f2, overwrite = FALSE)
  expect_true(file.exists(f2))
  storage_copy(f1, f2, overwrite = TRUE)
  expect_true(file.exists(f2))
  d1 <- tempfile()
  d2 <- tempfile()
  x <- file.path(d1, "x")
  dir_create(d1)
  file.create(x)
  storage_copy(d1, d2)
  expect_true(file.exists(x))
  expect_warning(storage_copy(d1, d2, warn = TRUE, overwrite = FALSE))
  storage_copy(d1, d2, merge = FALSE, overwrite = TRUE)
  storage_copy(d1, d2, merge = TRUE, overwrite = TRUE)
  expect_true(file.exists(x))
})

test_with_dir("safe_vec_c() (#1138)", {
  expect_equal(safe_vec_c(letters, letters), c(letters, letters))
  x <- lm(mpg ~ cyl, data = mtcars)
  y <- lm(mpg ~ wt, data = mtcars)
  expect_equal(safe_vec_c(x, y), list(x, y))
  expect_equal(safe_vec_c(x, letters[1]), list(x, letters[1]))
  expect_error(safe_vec_c(stop("sdklfjiole")), regexp = "sdklfjiole")
})
