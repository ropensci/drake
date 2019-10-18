drake_context("utils")

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
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(error_na(1), NA_character_)
  expect_false(error_false(1))
  expect_error(error_tibble_times(123))
})

test_with_dir("isolate_example()", {
  isolate_example("example", file.create("abc"))
  expect_false(file.exists("abc"))
})

test_with_dir("lifecycle", {
  stages <- c(
    "experimental",
    "maturing",
    "stable",
    "questioning",
    "retired",
    "archived",
    "soft-deprecated",
    "deprecated",
    "defunct"
  )
  for (stage in stages) {
    expect_true(is.character(lifecycle(stage)))
  }
  expect_error(lifecycle("not-a-stage"), regexp = "Unknown lifecycle stage")
})

test_with_dir("text wrapping", {
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
