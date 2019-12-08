drake_context("decorated storr")

test_with_dir("encoding empty keys", {
  x <- character(0)
  expect_equal(reencode_path(x), x)
  expect_equal(redecode_path(x), x)
  expect_equal(reencode_namespaced(x), x)
  expect_equal(redecode_namespaced(x), x)
})

test_with_dir("empty keys with decorated storr", {
  cache <- new_cache()
  x <- character(0)
  expect_equal(cache$encode_path(x), x)
  expect_equal(cache$decode_path(x), x)
  expect_equal(cache$encode_namespaced(x), x)
  expect_equal(cache$decode_namespaced(x), x)
})

test_with_dir("drake_tempfile()", {
  expect_error(drake_tempfile())
  cache <- new_cache()
  x <- drake_tempfile(cache = cache)
  expect_true(grepl("drake", x))
  expect_true(grepl("tmp", x))
})

test_with_dir("key encoding for paths and namespaced functions", {
  x <- c("myfunny:::variablename", "relative/path\na\\m//e")
  expect_false(all(is_encoded_path(x)))
  expect_false(all(is_encoded_namespaced(x)))

  y <- reencode_path(x)
  z <- reencode_namespaced(x)
  expect_false(any(y == z))

  expect_true(all(is_encoded_path(y)))
  expect_false(all(is_encoded_path(z)))

  expect_false(all(is_encoded_namespaced(y)))
  expect_true(all(is_encoded_namespaced(z)))

  expect_equal(redecode_path(y), x)
  expect_equal(redecode_namespaced(z), x)

  expect_true(all(file.create(y)))
  expect_true(all(file.create(z)))
})

test_with_dir("deco storr: key encoding for paths and namespaced fns", {
  cache <- new_cache()

  x <- c("myfunny:::variablename", "relative/path\na\\m//e")
  expect_false(all(is_encoded_path(x)))
  expect_false(all(is_encoded_namespaced(x)))

  y <- cache$encode_path(x)
  z <- cache$encode_namespaced(x)
  expect_false(any(y == z))

  expect_true(all(is_encoded_path(y)))
  expect_false(all(is_encoded_path(z)))

  expect_false(all(is_encoded_namespaced(y)))
  expect_true(all(is_encoded_namespaced(z)))

  expect_equal(cache$decode_path(y), x)
  expect_equal(cache$decode_namespaced(z), x)

  expect_true(all(file.create(y)))
  expect_true(all(file.create(z)))
})

test_with_dir("memoization encoding in decorated storr", {
  cache <- new_cache()
  x <- c("myfunny:::variablename", "relative/path\na\\m//e")
  expect_false(all(is_encoded_path(x)))
  expect_false(all(is_encoded_namespaced(x)))
  for (i in 1:3) {
    y <- cache$encode_path(x)
    z <- cache$encode_namespaced(x)
    expect_false(any(y == z))

    expect_true(all(is_encoded_path(y)))
    expect_false(all(is_encoded_path(z)))

    expect_false(all(is_encoded_namespaced(y)))
    expect_true(all(is_encoded_namespaced(z)))

    expect_equal(cache$decode_path(y), x)
    expect_equal(cache$decode_namespaced(z), x)

    expect_true(all(file.create(y)))
    expect_true(all(file.create(z)))
  }
  expect_equal(cache$decode_path("p-GEZDG"), "123")
  expect_equal(cache$decode_namespaced("n-GEZDG"), "123")
})

test_with_dir("redisplay keys", {
  expect_true(grepl("url", redisplay_keys(reencode_path("https://url"))))
  expect_true(grepl("file", redisplay_keys(reencode_path("123"))))
  expect_true(grepl("::", redisplay_keys(reencode_namespaced("pkg::fn"))))
})

test_with_dir("decorated storr: redisplay paths", {
  cache <- new_cache()
  cache$encode_path("https://url")
  expect_true(grepl("url", cache$display_keys(reencode_path("https://url"))))
  cache$encode_path("123")
  expect_true(grepl("file", cache$display_keys(reencode_path("123"))))
  cache$encode_namespaced("pkg::fn")
  expect_equal(cache$display_keys(reencode_namespaced("pkg::fn")), "pkg::fn")
})

test_with_dir("file_store quotes properly", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(file_store("x"), reencode_path("x"))
})

test_with_dir("run through non-encoder decorated storr methods", {
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
  x$fill("z", 2)
  x$flush_cache()
  x$get("x")
  x$get_value(x$get_hash("x"))
  x$hash_object("x")
  x$hash_raw("x")
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
  expect_false(inherits(ref2, "drake_format"))
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
  expect_true(inherits(ref, "drake_format"))
  expect_true(inherits(ref, "drake_format_rds"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
  ref2 <- cache$storr$get("y")
  expect_identical(ref2, "normal format")
  expect_false(inherits(ref2, "drake_format"))
  expect_false(inherits(ref2, "drake_format_rds"))
})

test_with_dir("rds format with hpc checksum", {
  skip_if(getRversion() < "3.5.0")
  skip_if_not_installed("future")
  future::plan(future::sequential)
  plan <- drake_plan(
    x = target(list(x = letters, y = letters), format = "rds"),
    y = "normal format"
  )
  make(plan, parallelism = "future", caching = "worker")
  out <- readd(x)
  exp <- list(x = letters, y = letters)
  expect_equal(out, exp)
  cache <- drake_cache()
  expect_equal(cache$get_value(cache$get_hash("x")), exp)
  ref <- cache$storr$get("x")
  expect_true(inherits(ref, "drake_format"))
  expect_true(inherits(ref, "drake_format_rds"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
  ref2 <- cache$storr$get("y")
  expect_identical(ref2, "normal format")
  expect_false(inherits(ref2, "drake_format"))
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
    expect_true(inherits(ref, "drake_format"))
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
  expect_true(inherits(ref, "drake_format"))
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
  expect_true(inherits(ref, "drake_format"))
  expect_true(inherits(ref, "drake_format_fst"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
})

test_with_dir("fst format forces data frames", {
  skip_if_not_installed("fst")
  plan <- drake_plan(
    x = target(
      list(x = letters, y = letters),
      format = "fst"
    )
  )
  expect_warning(make(plan), regexp = "plain data frame")
  expect_true(inherits(readd(x), "data.frame"))
})

test_with_dir("fst format and tibbles", {
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  plan <- drake_plan(
    x = target(
      tibble::tibble(x = letters, y = letters),
      format = "fst"
    ),
    y = class(x)
  )
  expect_warning(
    make(plan, memory_strategy = "speed"),
    regexp = "plain data frame"
  )
  expect_equal(readd(y), "data.frame")
  expect_false(inherits(readd(x), "tibble"))
})

test_with_dir("fst_dt", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("fst")
  plan <- drake_plan(
    x = target(
      data.table::as.data.table(
        data.frame(x = letters, y = letters, stringsAsFactors = FALSE)
      ),
      format = "fst_dt"
    )
  )
  make(plan)
  out <- readd(x)
  exp <- data.table::as.data.table(
    data.frame(x = letters, y = letters, stringsAsFactors = FALSE)
  )
  expect_equal(out, exp)
  cache <- drake_cache()
  expect_equal(cache$get_value(cache$get_hash("x")), exp)
  ref <- cache$storr$get("x")
  expect_true(inherits(ref, "drake_format"))
  expect_true(inherits(ref, "drake_format_fst_dt"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
})

test_with_dir("fst_dt format forces data.tables", {
  skip_if_not_installed("data.table")
  skip_if_not_installed("fst")
  plan <- drake_plan(
    x = target(
      list(x = letters, y = letters),
      format = "fst_dt"
    )
  )
  expect_warning(make(plan), regexp = "data.table")
  expect_true(inherits(readd(x), "data.table"))
})

test_with_dir("disk.frame (#1004)", {
  skip_if_not_installed("disk.frame")
  skip_if_not_installed("fst")
  plan <- drake_plan(
    x = target(
      disk.frame::as.disk.frame(
        data.frame(x = letters, y = letters, stringsAsFactors = FALSE),
        outdir = drake_tempfile()
      ),
      format = "diskframe"
    )
  )
  make(plan)
  out <- readd(x)
  expect_true(inherits(out, "disk.frame"))
  exp <- data.table::as.data.table(
    data.frame(x = letters, y = letters, stringsAsFactors = FALSE)
  )
  clean_up <- function(out) {
    out <- as.data.frame(out)
    out[order(out$x), ]
  }
  expect_equivalent(clean_up(out), exp)
  cache <- drake_cache()
  expect_equivalent(
    clean_up(cache$get_value(cache$get_hash("x"))),
    exp
  )
  ref <- cache$storr$get("x")
  expect_true(inherits(ref, "drake_format"))
  expect_true(inherits(ref, "drake_format_diskframe"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
})

test_with_dir("diskframe format forces disk.frames", {
  skip_if_not_installed("disk.frame")
  skip_if_not_installed("fst")
  plan <- drake_plan(
    x = target(
      data.frame(x = letters, y = letters),
      format = "diskframe"
    )
  )
  expect_warning(make(plan), regexp = "disk.frame")
  expect_true(inherits(readd(x), "disk.frame"))
})

test_with_dir("drop format for NULL values (#998)", {
  skip_if(getRversion() < "3.5.0")
  f <- function() {
    NULL
  }
  plan <- drake_plan(x = target(f(), format = "rds"))
  make(plan)
  expect_null(readd(x))
  expect_null(drake_cache()$storr$get("x"))
})

test_with_dir("decorated storr import (#1015)", {
  skip_if(getRversion() < "3.5.0")
  plan1 <- drake_plan(
    w = "w",
    x = "x",
    y = target("y", format = "rds"),
    z = target("z", format = "rds")
  )
  plan2 <- drake_plan(
    a = "a",
    x = "x",
    b = target("b", format = "rds"),
    z = target("z2", format = "rds")
  )
  cache1 <- new_cache("cache1")
  cache2 <- new_cache("cache2")
  make(plan1, cache = cache1)
  make(plan2, cache = cache2)
  cache1$import(cache2)
  expect_equal(cache1$get("a"), "a")
  expect_true(is.list(cache1$get("a", namespace = "meta")))
  expect_equal(cache1$get("b"), "b")
  expect_equal(cache1$get("z"), "z2")
})

test_with_dir("decorated storr export (#1015)", {
  skip_if(getRversion() < "3.5.0")
  plan1 <- drake_plan(
    w = "w",
    x = "x",
    y = target("y", format = "rds"),
    z = target("z", format = "rds")
  )
  plan2 <- drake_plan(
    a = "a",
    x = "x",
    b = target("b", format = "rds"),
    z = target("z2", format = "rds")
  )
  cache1 <- new_cache("cache1")
  cache2 <- new_cache("cache2")
  make(plan1, cache = cache1)
  make(plan2, cache = cache2)
  cache2$export(cache1, gc = FALSE)
  expect_equal(cache1$get("a"), "a")
  expect_true(is.list(cache1$get("a", namespace = "meta")))
  expect_equal(cache1$get("b"), "b")
  expect_equal(cache1$get("z"), "z2")
})

test_with_dir("decorated storr import specific targets (#1015)", {
  skip_if(getRversion() < "3.5.0")
  plan1 <- drake_plan(
    w = "w",
    x = "x",
    y = target("y", format = "rds"),
    z = target("z", format = "rds")
  )
  plan2 <- drake_plan(
    a = "a",
    x = "x",
    b = target("b", format = "rds"),
    z = target("z2", format = "rds")
  )
  cache1 <- new_cache("cache1")
  cache2 <- new_cache("cache2")
  make(plan1, cache = cache1)
  make(plan2, cache = cache2)
  cache1$import(cache2, a)
  expect_equal(cache1$get("a"), "a")
  expect_true(is.list(cache1$get("a", namespace = "meta")))
  expect_false(cache1$exists("b"))
  expect_equal(cache1$get("z"), "z")
})

test_with_dir("decorated storr import specific targets (#1015)", {
  skip_if(getRversion() < "3.5.0")
  plan1 <- drake_plan(
    w = "w",
    x = "x",
    y = target("y", format = "rds"),
    z = target("z", format = "rds")
  )
  plan2 <- drake_plan(
    a = "a",
    x = "x",
    b = target("b", format = "rds"),
    z = target("z2", format = "rds")
  )
  cache1 <- new_cache("cache1")
  cache2 <- new_cache("cache2")
  make(plan1, cache = cache1)
  make(plan2, cache = cache2)
  cache2$export(cache1, a)
  expect_equal(cache1$get("a"), "a")
  expect_true(is.list(cache1$get("a", namespace = "meta")))
  expect_false(cache1$exists("b"))
  expect_equal(cache1$get("z"), "z")
})

test_with_dir("safe_get*() methods", {
  cache <- new_cache(tempfile())
  for (ns in c(cache$default_namespace, "meta")) {
    expect_equal(cache$safe_get("x", namespace = ns), NA_character_)
    expect_equal(cache$safe_get_hash("x", namespace = ns), NA_character_)
    cache$set("a", "b", namespace = ns)
    expect_equal(cache$safe_get("a", namespace = ns), "b")
    expect_false(is.na(cache$safe_get_hash("a", namespace = ns)))
  }
})

test_with_dir("in-memory representation of disk.frame targets (#1077)", {
  n <- 200
  observations <- data.frame(
    type = sample(letters[seq_len(3)], n, replace = TRUE),
    size = runif(n),
    stringsAsFactors = FALSE
  )
  plan <- drake_plan(
    all_data = target(
      observations,
      format = "fst"
    ),
    result = as.data.frame(head(all_data))
  )
  make(plan)
  out <- readd(result)
  expect_equal(dim(out), c(6L, 2L))
})
