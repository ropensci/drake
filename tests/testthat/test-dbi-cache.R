drake_context("dbi cache")

test_with_dir("storr_dbi is usable", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  mydb <- DBI::dbConnect(RSQLite::SQLite(), "my-db.sqlite")
  cache <- storr_dbi(
    "dattbl", "keystbl", con = mydb, hash_algorithm = "murmur32") %>%
    configure_cache(
      long_hash_algo = "sha1",
      overwrite_hash_algos = TRUE
    )

  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism
  
  con <- load_basic_example(envir = e)
  con$cache$destroy()
  my_plan <- e$my_plan
  config <- drake_config(
    my_plan, envir = e,
    jobs = jobs, parallelism = parallelism,
    cache = cache
  )
  testrun(config)
  
  expect_false(file.exists(default_cache_path()))
  expect_equal(short_hash(cache), "murmur32")
  expect_equal(long_hash(cache), "sha1")
  expect_equal(sort(built(cache = cache)), sort(my_plan$target))
  expect_false(file.exists(default_cache_path()))
  expect_equal(outdated(config), character(0))
  expect_false(file.exists(default_cache_path()))
})
