drake_context("priority queue")

test_with_dir("empty queue", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- list(schedule = igraph::make_empty_graph(), verbose = 4)
  q <- new_priority_queue(config)
  expect_equal(sort(colnames(q$data)), sort(c("target", "ndeps", "priority")))
  expect_equal(nrow(q$data), 0)
})

test_with_dir("the priority queue works", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  targets <- c("foo", "bar", "baz", "Bob", "Amy", "Joe", "soup", "spren")
  ndeps <- c(8, 2, 3, 7, 4, 1, 7, 5)
  priorities <- c(rep(2, 4), rep(1, 4))
  ndeps[4] <- ndeps[7]
  expect_false(identical(ndeps, sort(ndeps, decreasing = TRUE)))
  x <- refclass_priority_queue$new(
    data = data.frame(
      target = targets,
      ndeps = ndeps,
      priority = priorities,
      stringsAsFactors = FALSE
    )
  )
  x$sort()
  expect_equal(sort(colnames(x$data)), sort(c("target", "ndeps", "priority")))
  y <- data.frame(
    target = c("Joe", "bar", "baz", "Amy", "spren", "soup", "Bob", "foo"),
    ndeps = c(1, 2, 3, 4, 5, 7, 7, 8),
    priority = c(1, 2, 2, 1, 1, 1, 2, 2),
    stringsAsFactors = FALSE
  )
  expect_equivalent(x$data, y)
  expect_null(x$peek0())
  expect_null(x$pop0())
  expect_equivalent(x$data, y)
  for (i in 1:2) {
    x$decrease_key(c("bar", "spren"))
  }
  for (i in 1:3) {
    x$decrease_key("spren")
  }
  y <- data.frame(
    target = c("spren", "bar", "Joe", "baz", "Amy", "soup", "Bob", "foo"),
    ndeps = c(0, 0, 1, 3, 4, 7, 7, 8),
    priority = c(1, 2, 1, 2, 1, 1, 2, 2),
    stringsAsFactors = FALSE
  )
  expect_equivalent(x$data, y)
  expect_equal(x$peek0(), "spren")
  expect_equal(sort(x$list0()), sort(c("bar", "spren")))
  expect_equivalent(x$data, y)
  expect_equal(x$pop0(), "spren")
  expect_equal(x$peek0(), "bar")
  expect_equivalent(x$data, y[-1, ])
  expect_equal(x$pop0(), "bar")
  expect_equivalent(x$data, y[-1:-2, ])
  expect_null(x$peek0())
  expect_null(x$pop0())
  expect_equivalent(x$data, y[-1:-2, ])

  priorities[targets == "bar"] <- 1
  priorities[targets == "spren"] <- 2
  x <- refclass_priority_queue$new(
    data = data.frame(
      target = targets,
      ndeps = ndeps,
      priority = priorities,
      stringsAsFactors = FALSE
    )
  )
  for (i in 1:2) {
    x$decrease_key(c("bar", "spren"))
  }
  for (i in 1:3) {
    x$decrease_key("spren")
  }
  y <- data.frame(
    target = c("bar", "spren", "Joe", "baz", "Amy", "soup", "Bob", "foo"),
    ndeps = c(0, 0, 1, 3, 4, 7, 7, 8),
    priority = c(1, 2, 1, 2, 1, 1, 2, 2),
    stringsAsFactors = FALSE
  )
  expect_equivalent(x$data, y)
  expect_equal(x$peek0(), "bar")
  expect_equivalent(x$data, y)
  expect_equal(x$pop0(), "bar")
  expect_equal(x$peek0(), "spren")
  expect_equivalent(x$data, y[-1, ])
  expect_equal(x$pop0(), "spren")
  expect_equivalent(x$data, y[-1:-2, ])
  expect_null(x$peek0())
  expect_null(x$pop0())
  expect_equivalent(x$data, y[-1:-2, ])

  expect_null(x$list0())
  z <- y[-1:-2, ]
  expect_true(all(c("soup", "Bob") %in% x$data$target))
  expect_equal(nrow(x$data), 6)
  x$remove(c("soup", "Bob"))
  expect_false(all(c("soup", "Bob") %in% x$data$target))
  expect_equal(nrow(x$data), 4)
  expect_equivalent(x$data, z[-4:-5, ])
  x$remove(c("soup", "Bob"))
  expect_equivalent(x$data, z[-4:-5, ])
})

test_with_dir("queues with priorities", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  load_mtcars_example()
  my_plan$priority <- seq_len(nrow(my_plan))
  config <- drake_config(my_plan, cache = storr::storr_environment())
  config$schedule <- config$graph
  q <- new_priority_queue(config)
  expect_true(all(diff(q$data$ndeps) >= 0))
  expect_equal(nrow(q$data), length(igraph::V(config$graph)))
  expect_equal(sum(is.finite(q$data$priority)), nrow(config$plan))
  config$schedule <- targets_graph(config)
  q <- new_priority_queue(config)
  expect_true(all(diff(q$data$ndeps) >= 0))
  expect_equal(sort(q$data$target), sort(config$plan$target))
  expect_true(all(is.finite(q$data$priority)))
  config$schedule <- imports_graph(config)
  q <- new_priority_queue(config)
  expect_true(all(diff(q$data$ndeps) >= 0))
  expect_equal(
    sort(q$data$target),
    sort(setdiff(igraph::V(config$graph)$name, config$plan$target))
  )
  expect_false(any(is.finite(q$data$priority)))
})
