drake_context("decorated storr")

# To do: reactivate tests
if (FALSE) {

test_with_dir("Can save fst data frames", {
  skip_if_not_installed("fst")
  plan <- drake_plan(
    x = return_fst(
      data.frame(x = letters, y = letters, stringsAsFactors = FALSE)
    )
  )
  make(plan)
  out <- readd(x)
  exp <- data.frame(x = letters, y = letters, stringsAsFactors = FALSE)
  expect_equal(out, exp)
  cache <- drake_cache()
  expect_true(inherits(cache$storr$get("x"), "saved_fst"))
})

test_with_dir("Can save keras models", {
  plan <- drake_plan(
    x = return_keras(
      structure(list("keras_model"), class = "keras.engine.training.Model")
    )
  )
  make(plan)
  out <- readd(x)
  exp <- structure(list("keras_model"), class = "keras.engine.training.Model")
  expect_equal(out, exp)
  cache <- drake_cache()
  expect_true(inherits(cache$storr$get("x"), "saved_keras"))
})

test_with_dir("can save rds", {
  plan <- drake_plan(
    x = return_rds(
      list(x = letters, y = letters)
    )
  )
  make(plan)
  out <- readd(x)
  exp <- list(x = letters, y = letters)
  expect_equal(out, exp)
  cache <- drake_cache()
  expect_true(inherits(cache$storr$get("x"), "saved_rds"))
})

test_with_dir("Can save rds with envir cache", {
  plan <- drake_plan(
    x = return_rds(
      list(x = letters, y = letters, stringsAsFactors = FALSE)
    )
  )
  cache <- storr::storr_environment()
  make(plan)
  out <- readd(x, cache = cache)
  expect_true(inherits(cache$storr$get("x"), "saved_rds"))
  exp <- list(x = letters, y = letters)
  expect_equal(out, exp)
  special <- file.path(".drake", "drake", "data")
  expect.true(file.exists(special))
  special_files <- list.files(special, pattern = ".*\\.rds$")
  expect_equal(length(special_files), 1L)
})

test_with_dir("garbage collection", {
  plan <- drake_plan(
    x = return_rds(
      list(x = letters, y = letters)
    )
  )
  make(plan)
  cache <- drake_cache()
  special <- file.path(".drake", "drake", "data")
  file <- list.files(special, pattern = ".*\\.rds$", full.names = TRUE)
  expect_true(file.exists(file))
  cache$gc()
  expect_true(file.exists(file))
  clean(cache = cache)
  expect_true(file.exists(file))
  clean(cache = cache, garbage_collection = TRUE)
  expect_false(file.exists(file))
})

test_with_dir("recovery with return_rds()", {
  plan <- drake_plan(
    x = {
      file.create("x")
      return_rds(list(x = letters))
    }
  )
  make(plan)
  expect_true(file.exists("x"))
  unlink("x")
  plan <- drake_plan(
    x = {
      file.create("y")
      return_rds(list(x = letters[1:3]))
    }
  )
  expect_true(file.exists("y"))
  unlink("y")
  expect_false(file.exists("x"))
  expect_false(file.exists("y"))
  plan <- drake_plan(
    x = {
      file.create("x")
      return_rds(list(x = letters))
    }
  )
  make(plan, recover = TRUE)
  expect_false(file.exists("x"))
  expect_equal(readd(x), list(x = letters, stringsAsFactors = FALSE))
  history <- drake_history()
  hash <- history[history$target == "x" & !history$current, "hash"]
  cache <- drake_cache()
  expect_equal(cache$get_value(hash), list(x = letters))
})

}

test_with_dir("runthrough of decorated storr methods", {
  skip_on_cran()
  x <- drake_config(drake_plan(x = 1))$cache
  x$archive_export(tempfile())
  x$archive_import(tempfile())
  x$check()
  x$clear()
  x$clone()
  x$set("x", 1)
  x$duplicate("x", "y")
  x$del("x")
  x$exists("x")
  x$exists_object("x")
  y <- storr::storr_environment()
  x$set("x", 1)
  x$export(y, "x")
  x$fill("z", 2)
  x$flush_cache()
  x$get("x")
  x$get_value(x$get_hash("x"))
  x$hash_object("x")
  x$hash_raw("x")
  x$import(y)
  x$index_export()
  x$index_import(x$index_export()[1:2, ])
  x$list()
  x$list_hashes()
  x$list_namespaces()
  x$mget("x")
  x$mget_hash("x")
  x$mget_value(x$get_hash("x"))
  x$mset("x", 1)
  x$mset_by_value("x")
  x$mset_value("x")
  x$repair()
  x$serialize_object("x")
  x$set_by_value("x")
  x$set_value("x")
  x$destroy()
})
