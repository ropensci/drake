drake_context("hash")

test_with_dir("illegal hashes", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- drake_plan(a = 1)
  expect_error(
    make(
      x,
      hash_algorithm = "no_such_algo_aslkdjfoiewlk",
      session_info = FALSE
    )
  )
})

test_with_dir("stress test storage hash", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("knitr")
  load_mtcars_example()
  con <- drake_config(
    my_plan, verbose = 0L, session_info = FALSE,
    cache = storr::storr_environment()
  )
  make(config = con)
  # Can debug storage_hash() to make sure hashing is skipped
  # at the appropriate times.
  for (file in file_store(c("report.Rmd"))) {
    expect_true(is.character(storage_hash(file, config = con, 0)))
    expect_true(is.character(storage_hash(file, config = con, Inf)))
  }
})

test_with_dir("same with a directory", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  dir.create("dir")
  writeLines("123", "dir/a.txt")
  writeLines("456", "dir/b.txt")
  plan <- drake_plan(x = file_in("dir"))
  con <- drake_config(
    plan, verbose = 0L, session_info = FALSE,
    cache = storr::storr_environment()
  )
  make(config = con)
  # Can debug storage_hash() to make sure hashing is skipped
  # at the appropriate times.
  for (file in file_store("dir")) {
    expect_true(is.character(storage_hash(file, config = con, 0)))
    expect_true(is.character(storage_hash(file, config = con, Inf)))
  }
})

test_with_dir("hashing decisions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_true(
    should_rehash_storage(
      new_mtime = 0,
      old_mtime = 0,
      old_size = 0,
      new_size = 0,
      size_cutoff = Inf
    )
  )
  for (i in c(0, 1)) {
    expect_false(
      should_rehash_storage(
        new_mtime = 0,
        old_mtime = i,
        old_size = 0,
        new_size = 0,
        size_cutoff = -Inf
      )
    )
    expect_true(
      should_rehash_storage(
        new_mtime = 0,
        old_mtime = i,
        old_size = 0,
        new_size = 0,
        size_cutoff = Inf
      )
    )
  }
  for (s in c(-Inf, Inf)) {
    expect_true(
      should_rehash_storage(
        new_mtime = 1,
        old_mtime = 0,
        old_size = 0,
        new_size = 0,
        size_cutoff = s
      )
    )
    expect_true(
      should_rehash_storage(
        new_mtime = 0,
        old_mtime = 0,
        old_size = 1,
        new_size = 0,
        size_cutoff = s
      )
    )
    expect_true(
      should_rehash_storage(
        new_mtime = 0,
        old_mtime = 0,
        old_size = 0,
        new_size = 1,
        size_cutoff = s
      )
    )
  }
})
