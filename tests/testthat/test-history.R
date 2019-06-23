drake_context("history")

test_with_dir("history with no cache", {
  expect_error(drake_history(), regexp = "cannot find drake cache")
})

test_with_dir("basic history", {
  skip_if_not_installed("txtq")
  # Iterate.
  load_mtcars_example()
  cache <- storr::storr_environment()
  make(my_plan, history = TRUE, cache = cache, session_info = FALSE)
  reg2 <- function(d) {
    d$x2 <- d$x ^ 3
    lm(y ~ x2, data = d)
  }
  make(my_plan, history = TRUE, cache = cache, session_info = FALSE)

  # Get and inspect the history.
  out <- drake_history(cache = cache, analyze = TRUE)
  expect_equal(dim(out), c(22L, 8L))
  cols <- c(
    "target",
    "time",
    "hash",
    "exists",
    "latest",
    "command",
    "runtime",
    "quiet"
  )
  expect_equal(sort(colnames(out)), sort(cols))
  expect_equal(sum(is.finite(out$quiet)), 2L)
  expect_true(all(out$quiet[out$target == "report"] == TRUE))

  # Recover an old version of a target.
  i <- which(out$target == "regression2_small")
  expect_equal(length(i), 2)
  hash_oldest_reg2_small <- out[max(i), ]$hash
  reg <- cache$get_value(hash_oldest_reg2_small)
  expect_true(inherits(reg, "lm"))

  # Clean
  clean(small, cache = cache)

  out <- drake_history(cache = cache)
})
