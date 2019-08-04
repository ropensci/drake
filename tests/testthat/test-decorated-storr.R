drake_context("decorated storr")

test_with_dir("Can save fst", {
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

test_with_dir("Can save keras", {
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

test_with_dir("other decorated cache fns", {
  config <- drake_config(drake_plan(x = 1))
  x <- config$cache
  x$set("a", "b")
  x$flush_cache()
  expect_equal(x$default_namespace, "objects")
  expect_equal(x$get("a"), "b")
  x$set("a", "c", namespace = "x")
  expect_equal(x$get("a", namespace = "x"), "c")
  expect_true(x$exists("a", "x"))
  x$mset(c("x", "y"), c("u", "v"), namespace = "tmp")
  o <- x$mget(c("x", "y"), namespace = "tmp")
  expect_equal(o, list("u", "v"))
  expect_true(x$exists("x", "tmp"))
  x$clear("tmp")
  expect_false(x$exists("x", "tmp"))
  expect_true(is.character(x$get_hash("a")))
  expect_true(is.character(x$get_hash("a", namespace = "x")))
  expect_false(x$get_hash("a") == x$get_hash("a", namespace = "x"))
  x$driver$set_hash("a", "z", x$get_hash("a"))
  expect_equal(x$get("a", namespace = "z"), "b")
  x$del("a", namespace = "z")
  expect_true(x$driver$exists_hash("a", namespace = "objects"))
  expect_true(x$driver$exists_hash("a", namespace = "x"))
  expect_false(x$driver$exists_hash("b", namespace = "x"))
  expect_true(x$driver$exists_object(x$get_hash("a")))
  expect_equal(x$driver$get_object(x$get_hash("a")), "b")
  expect_true(all(sort(c("objects", "x", "z")) %in% x$list_namespaces()))
  expect_equal(x$list(namespace = "x"), "a")
  expect_equal(x$driver$list_keys(namespace = "x"), "a")
  expect_true(is.character(x$list_hashes()))
  expect_true(length(x$list_hashes()) > 0L)
  hash <- x$get_hash("a")
  expect_true(x$exists_object(hash))
  file <- x$driver$name_hash(hash)
  expect_equal(dirname(file), file.path(x$driver$path, "data"))
  expect_true(file.exists(file))
  x$del("a")
  expect_false(x$exists("a"))
  expect_true(x$exists_object(hash))
  expect_true(file.exists(file))
  x$gc()
  expect_false(x$exists_object(hash))
  expect_false(file.exists(file))
  path <- x$driver$path
  expect_true(file.exists(path))
  x$destroy()
  expect_false(file.exists(path))
})
