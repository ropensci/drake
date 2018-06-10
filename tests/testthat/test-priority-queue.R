drake_context("priority queue")

test_with_dir("empty queue", {
  config <- list(schedule = igraph::make_empty_graph())
  q <- new_target_queue(config)
  expect_equal(nrow(q$data), 0)
})

test_with_dir("bad queue", {
  expect_error(
    R6_priority_queue$new(1:2, 1:4, 1:3),
    regexp = "priority queue"
  )
})

test_with_dir("the priority queue works", {
  targets <- c("foo", "bar", "baz", "Bob", "Amy", "Joe", "soup", "spren")
  ndeps <- c(8, 2, 3, 7, 4, 1, 7, 5)
  priorities <- c(rep(2, 4), rep(1, 4))
  ndeps[4] <- ndeps[7]
  expect_false(identical(ndeps, sort(ndeps, decreasing = TRUE)))
  expect_error(
    R6_priority_queue$new(
      targets = letters[1:2], ndeps = 1:4, priorities = 1:2))
  x <- R6_priority_queue$new(
    targets = targets, ndeps = ndeps, priorities = priorities)
  y <- data.frame(
    target = c("Joe", "bar", "baz", "Amy", "spren", "soup", "Bob", "foo"),
    ndeps = c(1, 2, 3, 4, 5, 7, 7, 8),
    priority = c(1, 2, 2, 1, 1, 1, 2, 2),
    stringsAsFactors = FALSE
  )
  expect_equal(x$data, y)
  expect_null(x$peek0())
  expect_null(x$pop0())
  expect_equal(x$data, y)
  for (i in 1:2){
    x$decrease_key(c("bar", "spren"))
  }
  for (i in 1:3){
    x$decrease_key("spren")
  }
  y <- data.frame(
    target = c("spren", "bar", "Joe", "baz", "Amy", "soup", "Bob", "foo"),
    ndeps = c(0, 0, 1, 3, 4, 7, 7, 8),
    priority = c(1, 2, 1, 2, 1, 1, 2, 2),
    stringsAsFactors = FALSE
  )
  expect_equal(x$data, y)
  expect_equal(x$peek0(), "spren")
  expect_equal(x$data, y)
  expect_equal(x$pop0(), "spren")
  expect_equal(x$peek0(), "bar")
  expect_equal(x$data, y[-1, ])
  expect_equal(x$pop0(), "bar")
  expect_equal(x$data, y[-1:-2, ])
  expect_null(x$peek0())
  expect_null(x$pop0())
  expect_equal(x$data, y[-1:-2, ])

  priorities[targets == "bar"] <- 1
  priorities[targets == "spren"] <- 2
  x <- R6_priority_queue$new(
    targets = targets, ndeps = ndeps, priorities = priorities)
  for (i in 1:2){
    x$decrease_key(c("bar", "spren"))
  }
  for (i in 1:3){
    x$decrease_key("spren")
  }
  y <- data.frame(
    target = c("bar", "spren", "Joe", "baz", "Amy", "soup", "Bob", "foo"),
    ndeps = c(0, 0, 1, 3, 4, 7, 7, 8),
    priority = c(1, 2, 1, 2, 1, 1, 2, 2),
    stringsAsFactors = FALSE
  )
  expect_equal(x$data, y)
  expect_equal(x$peek0(), "bar")
  expect_equal(x$data, y)
  expect_equal(x$pop0(), "bar")
  expect_equal(x$peek0(), "spren")
  expect_equal(x$data, y[-1, ])
  expect_equal(x$pop0(), "spren")
  expect_equal(x$data, y[-1:-2, ])
  expect_null(x$peek0())
  expect_null(x$pop0())
  expect_equal(x$data, y[-1:-2, ])
})

test_with_dir("queues with priorities", {
  load_mtcars_example(cache = storr::storr_environment())
  my_plan$priority <- seq_len(nrow(my_plan))
  config <- drake_config(my_plan)
  config$schedule <- config$graph
  q <- new_target_queue(config)
  expect_true(all(diff(q$data$ndeps) >= 0))
  expect_equal(nrow(q$data), length(igraph::V(config$graph)))
  expect_equal(sum(is.finite(q$data$priority)), nrow(config$plan))
  config$schedule <- targets_graph(config)
  q <- new_target_queue(config)
  expect_true(all(diff(q$data$ndeps) >= 0))
  expect_equal(sort(q$data$target), sort(config$plan$target))
  expect_true(all(is.finite(q$data$priority)))
  config$schedule <- imports_graph(config)
  q <- new_target_queue(config)
  expect_true(all(diff(q$data$ndeps) >= 0))
  expect_equal(
    sort(q$data$target),
    sort(setdiff(igraph::V(config$graph)$name, config$plan$target))
  )
  expect_false(any(is.finite(q$data$priority)))
})
