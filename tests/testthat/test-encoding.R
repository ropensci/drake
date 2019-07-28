drake_context("encoding")

test_with_dir("encoding empty keys", {
  x <- character(0)
  expect_equal(encode_path(x), x)
  expect_equal(decode_path(x), x)
  expect_equal(encode_namespaced(x), x)
  expect_equal(decode_namespaced(x), x)
})

test_with_dir("key encoding for paths and namespaced functions", {
  x <- c("myfunny:::variablename", "relative/path\na\\m//e")
  expect_false(all(is_encoded_path(x)))
  expect_false(all(is_encoded_namespaced(x)))

  y <- encode_path(x)
  z <- encode_namespaced(x)
  expect_false(any(y == z))

  expect_true(all(is_encoded_path(y)))
  expect_false(all(is_encoded_path(z)))

  expect_false(all(is_encoded_namespaced(y)))
  expect_true(all(is_encoded_namespaced(z)))

  expect_equal(decode_path(y), x)
  expect_equal(decode_namespaced(z), x)

  expect_true(all(file.create(y)))
  expect_true(all(file.create(z)))
})

test_with_dir("same with memoization", {
  config <- drake_config(
    drake_plan(targ = 1),
    cache = storr::storr_environment(),
    session_info = FALSE
  )
  x <- c("myfunny:::variablename", "relative/path\na\\m//e")
  expect_false(all(is_encoded_path(x)))
  expect_false(all(is_encoded_namespaced(x)))
  for (i in 1:3) {
    y <- encode_path(x, config = config)
    z <- encode_namespaced(x, config = config)
    expect_false(any(y == z))

    expect_true(all(is_encoded_path(y)))
    expect_false(all(is_encoded_path(z)))

    expect_false(all(is_encoded_namespaced(y)))
    expect_true(all(is_encoded_namespaced(z)))

    expect_equal(decode_path(y, config = config), x)
    expect_equal(decode_namespaced(z, config = config), x)

    expect_true(all(file.create(y)))
    expect_true(all(file.create(z)))
  }
})

test_with_dir("display paths", {
  expect_true(grepl("url", display_path(encode_path("https://url"), list())))
  expect_true(grepl("file", display_path(encode_path("123"), list())))
})

test_with_dir("file_store quotes properly", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(file_store("x"), encode_path("x"))
})
