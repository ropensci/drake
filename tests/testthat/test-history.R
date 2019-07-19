drake_context("history")

test_with_dir("edge_cases", {
  skip_if_not_installed("txtq")
  expect_error(drake_history(), regexp = "cannot find drake cache")
  expect_null(history_walk_args(NULL, NULL))
  cache <- storr::storr_environment()
  expect_error(drake_history(cache = cache), regexp = "no history")
})

test_with_dir("basic history", {
  skip_if_not_installed("txtq")
  # Iterate.
  load_mtcars_example()
  cache <- storr::storr_environment()
  make(
    my_plan,
    history = TRUE,
    cache = cache,
    session_info = FALSE,
    recoverable = FALSE
  )
  reg2 <- function(d) {
    d$x2 <- d$x ^ 3
    lm(y ~ x2, data = d)
  }
  Sys.sleep(0.01)
  make(
    my_plan,
    history = TRUE,
    cache = cache,
    session_info = FALSE,
    recoverable = FALSE
  )

  # Get and inspect the history.
  out <- drake_history(cache = cache, analyze = TRUE)
  expect_equal(dim(out), c(22L, 9L))
  cols <- c(
    "target",
    "current",
    "built",
    "hash",
    "exists",
    "command",
    "runtime",
    "seed",
    "quiet"
  )
  expect_equal(sort(colnames(out)), sort(cols))
  expect_equal(sum(is.finite(out[["quiet"]])), 2L)
  expect_true(all(out[["quiet"]][out$target == "report"] == TRUE))
  expect_true(is.integer(out$seed))

  # Without analysis
  x <- drake_history(cache = cache, analyze = FALSE)
  expect_false("quiet" %in% colnames(x))

  # Recover an old version of a target.
  i <- which(out$target == "regression2_small")
  expect_equal(length(i), 2)
  hash_oldest_reg2_small <- out[max(i), ]$hash
  reg <- cache$get_value(hash_oldest_reg2_small)
  expect_true(inherits(reg, "lm"))

  # Clean without garbage collection
  clean(small, cache = cache)
  out2 <- drake_history(cache = cache, analyze = TRUE)
  expect_equal(out2$current[out2$target == "small"], FALSE)
  out2$current[out2$target == "small"] <- TRUE
  expect_equal(out, out2)

  # After garbage collection
  cache$gc()
  out <- drake_history(cache = cache, analyze = TRUE)
  expect_equal(dim(out), c(22L, 9L))
  expect_equal(sort(colnames(out)), sort(cols))
  expect_equal(is.na(out$hash), !out$current)

  # Clean everything.
  clean(cache = cache, garbage_collection = TRUE)
  out <- drake_history(cache = cache, analyze = TRUE)
  expect_equal(dim(out), c(22L, 8L))
})

test_with_dir("complicated history commands", {
  skip_on_cran()
  skip_if_not_installed("txtq")
  plan <- drake_plan(
    a = identity(
      x = list(a = "x", 2, b = list(y = c(3L, 4L), z = sqrt(4), w = "5"))
    ),
    b = identity(
      x = list(a = "x", 2, b = list(y = 4L, z = sqrt(4), w = "5"))
    )
  )
  cache <- storr::storr_environment()
  make(plan, cache = cache, session_info = FALSE, history = TRUE)
  out <- drake_history(cache = cache, analyze = TRUE)
  expect_equal(ncol(out), 11L)
  expect_equal(out$a, rep("x", 2))
  expect_equal(out$w, rep("5", 2))
  expect_equal(out$y[2], 4L)
})

test_with_dir("file history", {
  skip_on_cran()
  skip_if_not_installed("txtq")
  plan <- drake_plan(
    x = c(list(
      a = file_in("a"),
      b = file_out("b"),
      c = knitr_in("c"),
      d = file_in("x", "y")
    ))
  )
  tmp <- file.create(c("a", "b", "c", "x", "y"))
  cache <- storr::storr_environment()
  make(plan, cache = cache, session_info = FALSE, history = TRUE)
  out <- drake_history(cache = cache, analyze = TRUE)
  for (x in letters[seq_len(3)]) {
    expect_equal(out[[x]], x)
  }
})
