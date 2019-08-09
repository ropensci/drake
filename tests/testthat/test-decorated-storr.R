drake_context("decorated storr")

test_with_dir("run through decorated storr methods", {
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

test_with_dir("garbage collection", {
  skip_if(getRversion() < "3.5.0")
  plan <- drake_plan(
    x = target(
      list(x = letters, y = letters),
      format = "rds"
    )
  )
  make(plan)
  cache <- drake_cache()
  special <- file.path(".drake", "drake", "return")
  file <- list.files(special, full.names = TRUE)
  expect_true(file.exists(file))
  cache$gc()
  expect_true(file.exists(file))
  clean(cache = cache)
  expect_true(file.exists(file))
  clean(cache = cache, garbage_collection = TRUE)
  expect_false(file.exists(file))
})

test_with_dir("no special format", {
  plan <- drake_plan(y = "normal format")
  make(plan)
  expect_identical(readd(y), "normal format")
  cache <- drake_cache()
  ref2 <- cache$storr$get("y")
  expect_identical(ref2, "normal format")
  expect_false(inherits(ref2, "drake_format_rds"))
})

test_with_dir("illegal format", {
  plan <- drake_plan(y = target("bad format", format = "bad format"))
  expect_error(
    drake_config(plan),
    regexp = "format column of your drake plan can only have values"
  )
})

test_with_dir("rds format", {
  skip_if(getRversion() < "3.5.0")
  plan <- drake_plan(
    x = target(list(x = letters, y = letters), format = "rds"),
    y = "normal format"
  )
  make(plan)
  out <- readd(x)
  exp <- list(x = letters, y = letters)
  expect_equal(out, exp)
  cache <- drake_cache()
  expect_equal(cache$get_value(cache$get_hash("x")), exp)
  ref <- cache$storr$get("x")
  expect_true(inherits(ref, "drake_format_rds"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
  ref2 <- cache$storr$get("y")
  expect_identical(ref2, "normal format")
  expect_false(inherits(ref2, "drake_format_rds"))
})

test_with_dir("flow with rds format", {
  skip_if(getRversion() < "3.5.0")
  plan <- drake_plan(
    x = target("a", format = "rds"),
    y = target(x, format = "rds")
  )
  config <- drake_config(plan)
  make(plan)
  for (target in c("x", "y")) {
    ref <- config$cache$storr$get("x")
    expect_true(inherits(ref, "drake_format_rds"))
  }
  expect_equal(sort(justbuilt(config)), sort(c("x", "y")))
  plan <- drake_plan(
    x = target(c("a"), format = "rds"),
    y = target(x, format = "rds")
  )
  make(plan)
  expect_equal(justbuilt(config), "x")
  make(plan)
  expect_equal(justbuilt(config), character(0))
  plan <- drake_plan(
    x = target("b", format = "rds"),
    y = target(x, format = "rds")
  )
  make(plan)
  expect_equal(sort(justbuilt(config)), sort(c("x", "y")))
})

test_with_dir("rds format with environment storr", {
  skip_if(getRversion() < "3.5.0")
  plan <- drake_plan(x = target(list(x = letters, y = letters), format = "rds"))
  cache <- storr::storr_environment()
  make(plan, cache = cache)
  out <- readd(x, cache = cache)
  cache <- drake_config(plan, cache = cache)$cache
  exp <- list(x = letters, y = letters)
  expect_equal(out, exp)
  expect_equal(cache$get_value(cache$get_hash("x")), exp)
  ref <- cache$storr$get("x")
  expect_true(inherits(ref, "drake_format_rds"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
  special <- file.path(".drake", "drake", "return")
  expect_true(file.exists(special))
  special_files <- list.files(special)
  expect_equal(length(special_files), 1L)
})

test_with_dir("rds format and recovery", {
  skip_if(getRversion() < "3.5.0")
  plan <- drake_plan(
    x = target({
      file.create("x")
      list(x = letters)
    },
    format = "rds")
  )
  make(plan)
  expect_true(file.exists("x"))
  unlink("x")
  plan <- drake_plan(
    x = target({
      file.create("y")
      list(x = letters[1:3])
    },
    format = "rds")
  )
  make(plan)
  expect_true(file.exists("y"))
  unlink("y")
  expect_false(file.exists("x"))
  expect_false(file.exists("y"))
  plan <- drake_plan(
    x = target({
      file.create("x")
      list(x = letters)
    },
    format = "rds")
  )
  make(plan, recover = TRUE)
  expect_false(file.exists("x"))
  expect_false(file.exists("y"))
  expect_equal(readd(x), list(x = letters))
  history <- drake_history()
  hash <- history[history$target == "x" & !history$current, ]$hash
  cache <- drake_cache()
  expect_equal(cache$get_value(hash), list(x = letters[1:3]))
})

test_with_dir("Can save fst data frames", {
  skip_if_not_installed("fst")
  plan <- drake_plan(
    x = target(
      data.frame(x = letters, y = letters, stringsAsFactors = FALSE),
      format = "fst"
    )
  )
  make(plan)
  out <- readd(x)
  exp <- data.frame(x = letters, y = letters, stringsAsFactors = FALSE)
  expect_equal(out, exp)
  cache <- drake_cache()
  expect_equal(cache$get_value(cache$get_hash("x")), exp)
  ref <- cache$storr$get("x")
  expect_true(inherits(ref, "drake_format_fst"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
})

test_with_dir("fst format requires data frames", {
  skip_if_not_installed("fst")
  plan <- drake_plan(
    x = target(
      list(x = letters, y = letters, stringsAsFactors = FALSE),
      format = "fst"
    )
  )
  expect_error(make(plan), regexp = "data frame")
})
