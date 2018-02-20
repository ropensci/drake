drake_context("dbi cache")

test_with_dir("storr_dbi is usable", {
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  fetch_cache <- drake_strings({
    mydb <- DBI::dbConnect(RSQLite::SQLite(), "my-db.sqlite")
    cache <- storr::storr_dbi(
      "dattbl", "keystbl", con = mydb, hash_algorithm = "murmur32")
    configure_cache(
      cache = cache,
      long_hash_algo = "sha1",
      overwrite_hash_algos = TRUE
    )
  })

  cache <- eval(parse(text = fetch_cache))
  cache2 <- this_cache(fetch_cache = fetch_cache)
  expect_equal(cache$list(), character(0))
  expect_equal(cache2$list(), character(0))
  on.exit({
    cache$driver$disconnect()
    cache2$driver$disconnect()
  })

  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  e$mydb <- mydb

  # Parallelism currently does not work here.
  # I need to debug.
  parallelism <- "mclapply"
  jobs <- 1
  # parallelism <- scenario$parallelism # nolint
  # jobs <- scenario$jobs # nolint

  con <- load_basic_example(envir = e)
  con$cache$destroy()

  # Need to fix richfitz/storr#60 before using the full workflow plan.
  e$my_plan <- e$my_plan[e$my_plan$target != "'report.md'", ]

  my_plan <- e$my_plan
  config <- drake_config(
    my_plan, envir = e,
    jobs = jobs,
    parallelism = parallelism,
    cache = cache,
    fetch_cache = fetch_cache
  )
  testrun(config)

  if (parallelism %in% parallelism_choices(distributed_only = TRUE)){
    expect_true(file.exists(default_cache_path()))
  } else {
    expect_false(file.exists(default_cache_path()))
  }

  expect_equal(short_hash(cache), "murmur32")
  expect_equal(long_hash(cache), "sha1")
  expect_equal(sort(built(cache = cache)), sort(config$plan$target))
  expect_equal(outdated(config), character(0))
})
