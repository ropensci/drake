drake_context("decorated storr")

test_with_dir("encoding empty keys", {
  skip_on_cran()
  x <- character(0)
  expect_equal(reencode_path(x), x)
  expect_equal(redecode_path(x), x)
  expect_equal(reencode_namespaced(x), x)
  expect_equal(redecode_namespaced(x), x)
})

test_with_dir("empty keys with decorated storr", {
  skip_on_cran()
  cache <- new_cache()
  x <- character(0)
  expect_equal(cache$encode_path(x), x)
  expect_equal(cache$decode_path(x), x)
  expect_equal(cache$encode_namespaced(x), x)
  expect_equal(cache$decode_namespaced(x), x)
})

test_with_dir("drake_tempfile()", {
  skip_on_cran()
  expect_error(drake_tempfile())
  cache <- new_cache()
  x <- drake_tempfile(cache = cache)
  expect_true(grepl("drake", x))
  expect_true(grepl("tmp", x))
})

test_with_dir("key encoding for paths and namespaced functions", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
  expect_true(grepl("url", redisplay_keys(reencode_path("https://url"))))
  expect_true(grepl("file", redisplay_keys(reencode_path("123"))))
  expect_true(grepl("::", redisplay_keys(reencode_namespaced("pkg::fn"))))
})

test_with_dir("decorated storr: redisplay paths", {
  skip_on_cran()
  cache <- new_cache()
  cache$encode_path("https://url")
  expect_true(grepl("url", cache$display_keys(reencode_path("https://url"))))
  cache$encode_path("123")
  expect_true(grepl("file", cache$display_keys(reencode_path("123"))))
  cache$encode_namespaced("pkg::fn")
  expect_equal(cache$display_keys(reencode_namespaced("pkg::fn")), "pkg::fn")
})

test_with_dir("file_store quotes properly", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_equal(file_store("x"), reencode_path("x"))
})

test_with_dir("run through non-encoder decorated storr methods", {
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
  plan <- drake_plan(y = target("bad format", format = "bad format"))
  expect_error(
    make(plan),
    regexp = "illegal format"
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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

test_with_dir("Can save fst_tbl tibbles (#1154)", {
  skip_on_cran()
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  plan <- drake_plan(
    x = target(
      tibble::tibble(x = letters, y = letters),
      format = "fst_tbl"
    )
  )
  make(plan)
  out <- readd(x)
  exp <- tibble::tibble(x = letters, y = letters)
  expect_equal(out, exp)
  cache <- drake_cache()
  expect_equal(cache$get_value(cache$get_hash("x")), exp)
  ref <- cache$storr$get("x")
  expect_true(inherits(ref, "drake_format"))
  expect_true(inherits(ref, "drake_format_fst_tbl"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
})

test_with_dir("fst format forces data frames", {
  skip_on_cran()
  skip_if_not_installed("fst")
  plan <- drake_plan(
    x = target(
      list(x = letters, y = letters),
      format = "fst"
    )
  )
  expect_warning(make(plan), regexp = "plain data frame")
  expect_true(inherits(readd(x), "data.frame"))
  expect_false(inherits(readd(x), "tbl_df"))
})

test_with_dir("regular fst format and tibbles", {
  skip_on_cran()
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

test_with_dir("fst_tbl format forces tibbles (#1154)", {
  skip_on_cran()
  skip_if_not_installed("fst")
  skip_if_not_installed("tibble")
  plan <- drake_plan(
    x = target(
      list(x = letters, y = letters),
      format = "fst_tbl"
    )
  )
  expect_warning(make(plan), regexp = "tibble")
  expect_true(inherits(readd(x), "tbl_df"))
})

test_with_dir("fst_dt", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  # https://github.com/xiaodaigh/disk.frame/issues/227
  suppressWarnings(make(plan))
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
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
  cache1$import(cache2, gc = FALSE)
  expect_equal(cache1$get("a"), "a")
  expect_true(is.list(cache1$get("a", namespace = "meta")))
  expect_equal(cache1$get("b"), "b")
  expect_equal(cache1$get("z"), "z2")
})

test_with_dir("decorated storr export (#1015)", {
  skip_on_cran()
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
  skip_on_cran()
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
  cache1$import(cache2, a, gc = FALSE)
  expect_equal(cache1$get("a"), "a")
  expect_true(is.list(cache1$get("a", namespace = "meta")))
  expect_false(cache1$exists("b"))
  expect_equal(cache1$get("z"), "z")
})

test_with_dir("decorated storr import specific targets (#1015)", {
  skip_on_cran()
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
  cache2$export(cache1, a, gc = FALSE)
  expect_equal(cache1$get("a"), "a")
  expect_true(is.list(cache1$get("a", namespace = "meta")))
  expect_false(cache1$exists("b"))
  expect_equal(cache1$get("z"), "z")
})

test_with_dir("safe_get*() methods", {
  skip_on_cran()
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
  skip_on_cran()
  skip_if_not_installed("disk.frame")
  skip_if_not_installed("fst")
  n <- 200
  observations <- data.frame(
    type = sample(letters[seq_len(3)], n, replace = TRUE),
    size = runif(n),
    stringsAsFactors = FALSE
  )
  plan <- drake_plan(
    all_data = target(
      disk.frame::as.disk.frame(observations, drake_tempfile()),
      format = "diskframe"
    ),
    result = as.data.frame(head(all_data))
  )
  make(plan)
  out <- readd(result)
  expect_equal(dim(out), c(6L, 2L))
})

test_with_dir("changes to formats invalidate targets (#1104)", {
  skip_on_cran()
  skip_if_not_installed("fst")
  df <- data.frame(x = letters, y = letters, stringsAsFactors = FALSE)
  plan <- drake_plan(x = df)
  config <- drake_config(plan)
  make(plan)
  expect_equal(justbuilt(config), "x")
  make(plan)
  expect_equal(justbuilt(config), character(0))
  plan <- drake_plan(x = target(df, format = "fst"))
  config <- drake_config(plan)
  make(plan)
  expect_equal(justbuilt(config), "x")
  skip_if(getRversion() < "3.5.0")
  plan <- drake_plan(x = target(df, format = "rds"))
  config <- drake_config(plan)
  make(plan)
  expect_equal(justbuilt(config), "x")
  plan <- drake_plan(x = df)
  config <- drake_config(plan)
  make(plan)
  expect_equal(justbuilt(config), "x")
})

test_with_dir("same with 2 targets (format is NA for x) (#1104)", {
  skip_on_cran()
  skip_if_not_installed("fst")
  df <- data.frame(x = letters, y = letters, stringsAsFactors = FALSE)
  plan <- drake_plan(x = df, y = x)
  config <- drake_config(plan)
  make(plan)
  expect_equal(sort(justbuilt(config)), sort(c("x", "y")))
  make(plan)
  expect_equal(justbuilt(config), character(0))
  plan <- drake_plan(x = df, y = target(x, format = "fst"))
  config <- drake_config(plan)
  make(plan)
  expect_equal(justbuilt(config), "y")
  skip_if(getRversion() < "3.5.0")
  plan <- drake_plan(x = df, y = target(x, format = "rds"))
  config <- drake_config(plan)
  make(plan)
  expect_equal(justbuilt(config), "y")
  plan <- drake_plan(x = df, y = x)
  config <- drake_config(plan)
  make(plan)
  expect_equal(justbuilt(config), "y")
})

test_with_dir("can suppress the format trigger (#1104)", {
  skip_on_cran()
  skip_if_not_installed("fst")
  df <- data.frame(x = letters, y = letters, stringsAsFactors = FALSE)
  plan <- drake_plan(x = target(df))
  config <- drake_config(plan)
  make(plan)
  expect_equal(justbuilt(config), "x")
  make(plan)
  expect_equal(justbuilt(config), character(0))
  plan <- drake_plan(
    x = target(df, format = "fst", trigger = trigger(format = FALSE))
  )
  config <- drake_config(plan)
  make(plan)
  expect_equal(justbuilt(config), character(0))
  skip_if(getRversion() < "3.5.0")
  plan <- drake_plan(
    x = target(df, format = "rds", trigger = trigger(format = FALSE))
  )
  config <- drake_config(plan)
  make(plan)
  expect_equal(justbuilt(config), character(0))
})

test_with_dir("$import() copies (does not simply move) (#1120)", {
  skip_on_cran()
  skip_if_not_installed("fst")
  skip_if_not_installed("disk.frame")
  cache1 <- new_cache("cache1")
  cache2 <- new_cache("cache2")
  plan <- drake_plan(
    data = data.frame(
      x = runif(1000),
      y = runif(1000)
    ),
    data_fst = target(
      data,
      format = "fst"
    ),
    data_disk = target(
      disk.frame::as.disk.frame(
        rlang::duplicate(data),
        outdir = drake_tempfile(cache = cache1)
      ),
      format = "diskframe"
    )
  )
  make(plan, cache = cache1)
  cache2$import(data, from = cache1, gc = FALSE)
  expect_equal(
    readd(data, cache = cache1),
    readd(data, cache = cache2)
  )
  cache2$import(data_fst, from = cache1, gc = FALSE)
  expect_equal(
    readd(data_fst, cache = cache1),
    readd(data_fst, cache = cache2)
  )
  cache2$import(data_disk, from = cache1, gc = FALSE)
  x1 <- as.data.frame(readd(data_disk, cache = cache1))
  x2 <- as.data.frame(readd(data_disk, cache = cache2))
  x1 <- x1[order(x1$x), ]
  x2 <- x2[order(x2$x), ]
  expect_equal(x1, x2)
})

test_with_dir("qs format (#1121)", {
  skip_on_cran()
  skip_if_not_installed("qs")
  plan <- drake_plan(
    x = target(list(x = letters, y = letters), format = "qs"),
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
  expect_true(inherits(ref, "drake_format_qs"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
  ref2 <- cache$storr$get("y")
  expect_identical(ref2, "normal format")
  expect_false(inherits(ref2, "drake_format"))
  expect_false(inherits(ref2, "drake_format_qs"))
})

test_with_dir("global rds format (#1124)", {
  skip_on_cran()
  skip_if(getRversion() < "3.5.0")
  plan <- drake_plan(
    x = list(x = letters, y = letters),
    y = "rds format"
  )
  make(plan, format = "rds")
  out <- readd(x)
  exp <- list(x = letters, y = letters)
  expect_equal(out, exp)
  expect_equal(readd(y), "rds format")
  cache <- drake_cache()
  expect_equal(cache$get_value(cache$get_hash("x")), exp)
  for (key in c("x", "y")) {
    ref <- cache$storr$get(key)
    expect_true(inherits(ref, "drake_format"))
    expect_true(inherits(ref, "drake_format_rds"))
    expect_equal(length(ref), 1L)
    expect_true(nchar(ref) < 100)
    expect_false(is.list(ref))
  }
})

test_with_dir("global rds format + target qs (#1124)", {
  skip_on_cran()
  skip_if_not_installed("qs")
  skip_if(getRversion() < "3.5.0")
  plan <- drake_plan(
    x = list(x = letters, y = letters),
    y = target("qs format", format = "qs")
  )
  make(plan, format = "rds")
  out <- readd(x)
  exp <- list(x = letters, y = letters)
  expect_equal(out, exp)
  expect_equal(readd(y), "qs format")
  cache <- drake_cache()
  expect_equal(cache$get_value(cache$get_hash("x")), exp)
  ref <- cache$storr$get("x")
  expect_true(inherits(ref, "drake_format"))
  expect_true(inherits(ref, "drake_format_rds"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
  ref <- cache$storr$get("y")
  expect_true(inherits(ref, "drake_format"))
  expect_true(inherits(ref, "drake_format_qs"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
})

test_with_dir("file format with flat files and static targets (#1168)", {
  skip_on_cran()
  write_lines <- function(files, ...) {
    for (file in files) {
      writeLines(c(file, "stuff"), file)
    }
    files
  }
  plan <- drake_plan(
    w = 1,
    x = target(
      write_lines(c("a", "b")),
      format = "file"
    ),
    y = target(
      write_lines(c("c", "d"), x),
      format = "file"
    ),
    z = y
  )
  # initial state
  config <- drake_config(plan, history = FALSE)
  expect_equal(sort(outdated_impl(config)), sort(c("w", "x", "y", "z")))
  make_impl(config)
  expect_equal(sort(justbuilt(config)), sort(c("w", "x", "y", "z")))
  # file format internals
  expect_identical(readd(x), c("a", "b"))
  expect_identical(config$cache$get("x"), c("a", "b"))
  hash <- config$cache$get_hash("x")
  expect_identical(config$cache$get_value(hash), c("a", "b"))
  expect_false("history" %in% list.files(".drake/drake"))
  val <- config$cache$storr$get("x")
  val2 <- config$cache$storr$get_value(hash)
  expect_identical(val, val2)
  expect_equal(val$value, c("a", "b"))
  expect_true(is.character(val$hash))
  expect_equal(length(val$hash), 2)
  expect_true(inherits(val, "drake_format_file"))
  expect_true(inherits(val, "drake_format"))
  # validated state
  expect_equal(outdated_impl(config), character(0))
  make_impl(config)
  expect_equal(justbuilt(config), character(0))
  # write the same content to an x file
  write_lines("b")
  expect_equal(outdated_impl(config), character(0))
  make_impl(config)
  expect_equal(justbuilt(config), character(0))
  # corrupt an x file
  writeLines("123", "b")
  expect_equal(readLines("b"), "123")
  expect_equal(sort(outdated_impl(config)), sort(c("x", "y", "z")))
  make_impl(config)
  expect_equal(justbuilt(config), "x")
  expect_equal(readLines("b"), c("b", "stuff"))
  # remove an x file
  unlink("b")
  expect_false(file.exists("b"))
  expect_equal(sort(outdated_impl(config)), sort(c("x", "y", "z")))
  make_impl(config)
  expect_equal(justbuilt(config), "x")
  expect_equal(readLines("b"), c("b", "stuff"))
  # corrupt x and y files
  writeLines("123", "b")
  writeLines("123", "c")
  expect_equal(readLines("b"), "123")
  expect_equal(readLines("c"), "123")
  expect_equal(sort(outdated_impl(config)), sort(c("x", "y", "z")))
  make_impl(config)
  expect_equal(justbuilt(config), c("x", "y"))
  expect_equal(readLines("b"), c("b", "stuff"))
  expect_equal(readLines("c"), c("c", "stuff"))
  # change the expected file content
  write_lines <- function(files, ...) {
    for (file in files) {
      writeLines(c(file, "new stuff"), file)
    }
    files
  }
  expect_equal(sort(outdated_impl(config)), sort(c("x", "y", "z")))
  make_impl(config)
  expect_equal(justbuilt(config), c("x", "y", "z"))
  expect_equal(readLines("b"), c("b", "new stuff"))
  # same with a shorter plan
  clean(destroy = TRUE)
  plan <- drake_plan(
    x = target(
      write_lines(c("a", "b")),
      format = "file"
    ),
    y = readLines(x[1])
  )
  make(plan)
  write_lines <- function(files, ...) {
    for (file in files) {
      writeLines(c(file, "newer stuff"), file)
    }
    files
  }
  config <- drake_config(plan)
  make(plan)
  expect_equal(sort(justbuilt(config)), sort(c("x", "y")))
  expect_equal(readd(y), c("a", "newer stuff"))
})

test_with_dir("file format with directories and static targets (#1168)", {
  skip_on_cran()
  write_lines <- function(files, ...) {
    for (file in files) {
      if (!dir.exists(file)) {
        dir.create(file)
      }
      writeLines(c(file, "stuff"), file.path(file, "x"))
      writeLines(c(file, "stuff"), file.path(file, "y"))
    }
    files
  }
  plan <- drake_plan(
    w = 1,
    x = target(
      write_lines(c("a", "b")),
      format = "file"
    ),
    y = target(
      write_lines(c("c", "d"), x),
      format = "file"
    ),
    z = y
  )
  # initial state
  config <- drake_config(plan, history = FALSE)
  expect_equal(sort(outdated_impl(config)), sort(c("w", "x", "y", "z")))
  make_impl(config)
  expect_equal(sort(justbuilt(config)), sort(c("w", "x", "y", "z")))
  # file format internals
  expect_identical(readd(x), c("a", "b"))
  expect_identical(config$cache$get("x"), c("a", "b"))
  hash <- config$cache$get_hash("x")
  expect_identical(config$cache$get_value(hash), c("a", "b"))
  expect_false("history" %in% list.files(".drake/drake"))
  val <- config$cache$storr$get("x")
  val2 <- config$cache$storr$get_value(hash)
  expect_identical(val, val2)
  expect_equal(val$value, c("a", "b"))
  expect_true(is.character(val$hash))
  expect_equal(length(val$hash), 2)
  expect_true(inherits(val, "drake_format_file"))
  expect_true(inherits(val, "drake_format"))
  # validated state
  expect_equal(outdated_impl(config), character(0))
  make_impl(config)
  expect_equal(justbuilt(config), character(0))
  # write the same content to an x file
  write_lines("b")
  expect_equal(outdated_impl(config), character(0))
  make_impl(config)
  expect_equal(justbuilt(config), character(0))
  # corrupt an x file
  writeLines("123", file.path("b", "y"))
  expect_equal(readLines(file.path("b", "y")), "123")
  expect_equal(sort(outdated_impl(config)), sort(c("x", "y", "z")))
  make_impl(config)
  expect_equal(justbuilt(config), "x")
  expect_equal(readLines(file.path("b", "y")), c("b", "stuff"))
  # corrupt x and y files
  writeLines("123", file.path("b", "y"))
  writeLines("123", file.path("c", "y"))
  expect_equal(readLines(file.path("b", "y")), "123")
  expect_equal(readLines(file.path("c", "y")), "123")
  expect_equal(sort(outdated_impl(config)), sort(c("x", "y", "z")))
  make_impl(config)
  expect_equal(justbuilt(config), c("x", "y"))
  expect_equal(readLines(file.path("b", "y")), c("b", "stuff"))
  expect_equal(readLines(file.path("c", "y")), c("c", "stuff"))
  # change the expected file content
  write_lines <- function(files, ...) {
    for (file in files) {
      if (!dir.exists(file)) {
        dir.create(file)
      }
      writeLines(c(file, "new stuff"), file.path(file, "x"))
      writeLines(c(file, "new stuff"), file.path(file, "y"))
    }
    files
  }
  expect_equal(sort(outdated_impl(config)), sort(c("x", "y", "z")))
  make_impl(config)
  expect_equal(justbuilt(config), c("x", "y", "z"))
  expect_equal(readLines(file.path("b", "y")), c("b", "new stuff"))
})

test_with_dir("bad file format value", {
  skip_on_cran()
  f <- function() {
    writeLines("1", "1")
    1
  }
  plan <- drake_plan(x = target(f(), format = "file"))
  expect_warning(make(plan), regexp = "character")
})

test_with_dir("file trigger and dynamic files (#1168)", {
  skip_on_cran()
  write_lines <- function(files, ...) {
    for (file in files) {
      writeLines(c(file, "stuff"), file)
    }
    files
  }
  plan <- drake_plan(
    x = target(
      write_lines(c("a", "b")),
      format = "file"
    )
  )
  make(plan)
  unlink(c("a", "b"))
  config <- drake_config(plan)
  make(plan, trigger = trigger(file = FALSE))
  expect_equal(justbuilt(config), character(0))
})

test_with_dir("data recovery and dynamic files (#1168)", {
  skip_on_cran()
  write_lines <- function(files, ...) {
    for (file in files) {
      writeLines(c(file, "stuff"), file)
    }
    files
  }
  f <- function() {
    write_lines("no_recover")
    write_lines("b")
  }
  plan <- drake_plan(
    x = target(f(), format = "file")
  )
  make(plan)
  unlink("no_recover")
  config <- drake_config(plan)
  # Clean, remove output file, and fail to recover.
  clean()
  unlink(c("no_recover", "b"))
  r <- recoverable(plan)
  expect_equal(r, character(0))
  make(plan, recover = TRUE)
  expect_equal(justbuilt(config), "x")
  expect_true(file.exists("no_recover"))
  expect_true(file.exists("b"))
  # Set up to restore file and recover old target.
  write_lines <- function(files, ...) {
    for (file in files) {
      writeLines(c(file, "stuff2"), file)
    }
    files
  }
  unlink("no_recover")
  make(plan)
  expect_equal(justbuilt(config), "x")
  expect_true(file.exists("no_recover"))
  expect_true(file.exists("b"))
  expect_equal(outdated_impl(config), character(0))
  make(plan)
  expect_equal(justbuilt(config), character(0))
  # Restore file and recover old target.
  write_lines <- function(files, ...) {
    for (file in files) {
      writeLines(c(file, "stuff"), file)
    }
    files
  }
  unlink("no_recover")
  write_lines("b")
  r <- recoverable(plan)
  expect_equal(r, "x")
  make(plan, recover = TRUE)
  expect_equal(justbuilt(config), "x")
  expect_false(file.exists("no_recover"))
  expect_true(file.exists("b"))
  expect_equal(outdated_impl(config), character(0))
})

test_with_dir("format file hpc checksums (#1168)", {
  skip_on_cran()
  skip_if_not_installed("future")
  write_lines <- function(files, ...) {
    for (file in files) {
      writeLines(c(file, "stuff"), file)
    }
    files
  }
  plan <- drake_plan(
    x = target(
      write_lines(c("a", "b")),
      format = "file"
    )
  )
  make(plan, parallelism = "future")
  config <- drake_config(plan)
  out <- format_file_checksum("x", readd(x), config)
  expect_equal(length(out), 2L)
  expect_equal(nchar(out), c(16L, 16L))
  clean(destroy = TRUE)
  make(plan, parallelism = "future", caching = "worker")
  skip_if_not_installed("clustermq", minimum_version = "0.9.1")
  skip_on_os("windows")
  options(clustermq.scheduler = "multicore")
  for (caching in c("main", "worker")) {
    clean(destroy = TRUE)
    make(plan, parallelism = "clustermq", caching = caching)
    config <- drake_config(plan)
    expect_equal(justbuilt(config), "x")
  }
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
})

test_with_dir("missing format file (#1168)", {
  skip_on_cran()
  plan <- drake_plan(x = target(c("a", "b"), format = "file"))
  expect_warning(make(plan), regexp = "missing dynamic files")
  out <- drake_cache()$storr$get("x")$hash
  exp <- rep(NA_character_, 2L)
  expect_equal(out, exp)
})

test_with_dir("empty format file (#1168)", {
  skip_on_cran()
  plan <- drake_plan(x = target(character(0), format = "file"))
  make(plan)
  out <- drake_cache()$storr$get("x")
  expect_equal(out$value, character(0))
  expect_equal(out$hash, character(0))
  expect_true(inherits(out, "drake_format_file"))
  expect_true(inherits(out, "drake_format"))
})

test_with_dir("non-character format file (#1168)", {
  skip_on_cran()
  plan <- drake_plan(x = target(1, format = "file"))
  expect_warning(make(plan), regexp = "coercing")
})

test_with_dir("mix of dynamic files and dirs (#1168)", {
  skip_on_cran()
  write_file <- function() {
    writeLines("stuff", "x")
    "x"
  }
  write_dir <- function() {
    if (!file.exists("y")) {
      dir.create("y")
    }
    writeLines("stuff", file.path("y", "z"))
    "y"
  }
  write_stuff <- function() {
    c(write_file(), write_dir())
  }
  plan <- drake_plan(
    x = target(write_stuff(), format = "file"),
    y = x
  )
  make(plan)
  config <- drake_config(plan)
  make(plan)
  expect_equal(justbuilt(config), character(0))
  write_file <- function() {
    writeLines("stuff2", "x")
    "x"
  }
  make(plan)
  expect_equal(sort(justbuilt(config)), sort(c("x", "y")))
  write_dir <- function() {
    if (!file.exists("y")) {
      dir.create("y")
    }
    writeLines("stuff2", file.path("y", "z"))
    "y"
  }
  make(plan)
  expect_equal(sort(justbuilt(config)), sort(c("x", "y")))
})

test_with_dir("keep_going for formatted targets (#1206)", {
  skip_on_cran()
  skip_if_not_installed("fst")
  plan <- drake_plan(
    x = target(stop(123), format = "fst"),
    y = target(stop(123), format = "fst")
  )
  make(plan, keep_going = TRUE)
  expect_equal(sort(drake_failed()), sort(c("x", "y")))
  expect_true(inherits(diagnose(x)$error, "simpleError"))
  expect_true(inherits(diagnose(y)$error, "simpleError"))
})
