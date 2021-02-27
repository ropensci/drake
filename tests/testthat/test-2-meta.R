drake_context("meta")

test_with_dir("stress test storage hash", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("tibble")
  skip_if_not_installed("knitr")
  load_mtcars_example()
  con <- drake_config(
    my_plan, verbose = 0L, session_info = FALSE,
    cache = storr::storr_environment()
  )
  make_impl(config = con)
  # Can debug static_storage_hash() to make sure hashing is skipped
  # at the appropriate times.
  for (file in file_store(c("report.Rmd"))) {
    expect_true(is.character(static_storage_hash(file, config = con, 0)))
    expect_true(is.character(static_storage_hash(file, config = con, Inf)))
  }
})

test_with_dir("same with a directory", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("tibble")
  dir.create("dir")
  writeLines("123", "dir/a.txt")
  writeLines("456", "dir/b.txt")
  plan <- drake_plan(x = file_in("dir"))
  con <- drake_config(
    plan, verbose = 0L, session_info = FALSE,
    cache = storr::storr_environment()
  )
  make_impl(config = con)
  # Can debug static_storage_hash() to make sure hashing is skipped
  # at the appropriate times.
  for (file in file_store("dir")) {
    expect_true(is.character(static_storage_hash(file, config = con, 0)))
    expect_true(is.character(static_storage_hash(file, config = con, Inf)))
  }
})

test_with_dir("hashing decisions", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("tibble")
  expect_true(
    should_rehash_local(
      new_mtime = 0,
      old_mtime = 0,
      old_size = 0,
      new_size = 0,
      size_threshold = Inf
    )
  )
  for (i in c(0, 1)) {
    expect_false(
      should_rehash_local(
        new_mtime = 0,
        old_mtime = i,
        old_size = 0,
        new_size = 0,
        size_threshold = -Inf
      )
    )
    expect_true(
      should_rehash_local(
        new_mtime = 0,
        old_mtime = i,
        old_size = 0,
        new_size = 0,
        size_threshold = Inf
      )
    )
  }
  for (s in c(-Inf, Inf)) {
    expect_true(
      should_rehash_local(
        new_mtime = 1,
        old_mtime = 0,
        old_size = 0,
        new_size = 0,
        size_threshold = s
      )
    )
    expect_true(
      should_rehash_local(
        new_mtime = 0,
        old_mtime = 0,
        old_size = 1,
        new_size = 0,
        size_threshold = s
      )
    )
    expect_true(
      should_rehash_local(
        new_mtime = 0,
        old_mtime = 0,
        old_size = 0,
        new_size = 1,
        size_threshold = s
      )
    )
  }
})

test_with_dir("storage hash of a non-existent path", {
  skip_if_not_installed("tibble")
  expect_false(file.exists("asdf"))
  config <- drake_config(drake_plan(x = 1))
  expect_true(is.na(static_storage_hash("asdf", config = config)))
  expect_true(is.na(rehash_static_storage("asdf", config = config)))
  expect_true(
    is.na(static_storage_hash(reencode_path("asdf"), config = config))
  )
  expect_true(
    is.na(rehash_static_storage(reencode_path("asdf"), config = config))
  )
})
