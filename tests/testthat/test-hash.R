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

test_with_dir("stress test hashing decisions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  file <- "nobodyhome"
  expect_false(file.exists(file))
  expect_true(should_rehash_storage(
    filename = file, new_mtime = 0, old_mtime = 0, size_cutoff = Inf))
  expect_true(should_rehash_storage(
    filename = file, new_mtime = 1, old_mtime = 0, size_cutoff = Inf))
  expect_true(should_rehash_storage(
    filename = file, new_mtime = 0, old_mtime = 1, size_cutoff = Inf))
  expect_true(should_rehash_storage(
    filename = file, new_mtime = 0, old_mtime = 0, size_cutoff = -1))
  expect_true(should_rehash_storage(
    filename = file, new_mtime = 1, old_mtime = 0, size_cutoff = -1))
  expect_true(should_rehash_storage(
    filename = file, new_mtime = 0, old_mtime = 1, size_cutoff = -1))
})

test_with_dir("more stress testing of hashing decisions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  file <- "input.rds"
  saveRDS(1, file = file)
  expect_true(file.exists(file))
  expect_true(should_rehash_storage(
    filename = file, new_mtime = 1, old_mtime = 0, size_cutoff = Inf))
  expect_true(should_rehash_storage(
    filename = file, new_mtime = 0, old_mtime = 1, size_cutoff = Inf))
  expect_true(should_rehash_storage(
    filename = file, new_mtime = 0, old_mtime = 0, size_cutoff = Inf))
  expect_true(should_rehash_storage(
    filename = file, new_mtime = 1, old_mtime = 0, size_cutoff = -1))
  expect_false(should_rehash_storage(
    filename = file, new_mtime = 0, old_mtime = 1, size_cutoff = -1))
  expect_false(should_rehash_storage(
    filename = file, new_mtime = 0, old_mtime = 0, size_cutoff = -1))
})

test_with_dir("same with a directory", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  file <- "myinputdir"
  dir.create(file)
  writeLines("12sddsfff3", "myinputdir/a.txt")
  writeLines("4sdfasdf56", "myinputdir/b.txt")
  expect_true(file.exists(file))
  expect_true(should_rehash_storage(
    filename = file, new_mtime = 1, old_mtime = 0, size_cutoff = Inf))
  expect_true(should_rehash_storage(
    filename = file, new_mtime = 0, old_mtime = 1, size_cutoff = Inf))
  expect_true(should_rehash_storage(
    filename = file, new_mtime = 0, old_mtime = 0, size_cutoff = Inf))
  expect_true(should_rehash_storage(
    filename = file, new_mtime = 1, old_mtime = 0, size_cutoff = -1))
  expect_false(should_rehash_storage(
    filename = file, new_mtime = 0, old_mtime = 1, size_cutoff = -1))
  expect_false(should_rehash_storage(
    filename = file, new_mtime = 0, old_mtime = 0, size_cutoff = -1))
})
