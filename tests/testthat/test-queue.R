drake_context("queue")

test_with_dir("empty queue", {
  config <- list(schedule = igraph::make_empty_graph())
  q <- new_target_queue(config)
  expect_equal(length(q$list()), 0)
})

test_with_dir("the priority queue works", {
  names <- c("foo", "bar", "baz", "Bob", "Amy", "Joe", "soup", "spren")
  priorities <- c(8, 2, 3, 7, 4, 1, 7, 5)
  priorities[4] <- priorities[7]
  expect_false(identical(priorities, sort(priorities, decreasing = TRUE)))
  expect_error(R6_priority_queue$new(names = letters[1:2], priorities = 1:4))
  x <- R6_priority_queue$new(names = names, priorities = priorities)
  expect_equal(x$list(what = "names"), names[order(priorities)])
  expect_equal(
    unname(x$list(what = "priorities")),
    sort(priorities, decreasing = FALSE)
  )

  expect_equal(x$size(), length(names))
  expect_false(x$empty())
  expect_false(x$any0())
  expect_true(!length(x$pop0()))
  elts <- x$list()
  head <- x$peek(n = 2)
  expect_equal(head, c("Joe", "bar"))
  expect_equal(unname(x$peek(n = 2, what = "priorities")), 1:2)
  expect_equal(x$list(what = "names"), names[order(priorities)])
  expect_equal(
    unname(x$list(what = "priorities")),
    sort(priorities, decreasing = FALSE)
  )

  expect_equal(x$pop(n = 2), head)
  expect_equal(x$list(what = "names"), names[order(priorities)][-1:-2])
  expect_equal(
    unname(x$list(what = "priorities")),
    sort(priorities, decreasing = FALSE)[-1:-2]
  )
  x$push(names = c("Preservation", "Ruin"), priorities = c(0, 6.66))
  expect_true(x$any0())
  nms <- c("Preservation", "baz", "Amy", "spren", "Ruin", "Bob", "soup", "foo")
  pri <- c(0, 3, 4, 5, 6.66, 7, 7, 8)
  expect_equal(x$list("names"), nms)
  expect_equal(unname(x$list("priorities")), pri)
  x$push()
  expect_equal(x$list("names"), nms)
  expect_equal(unname(x$list("priorities")), pri)

  expect_equal(x$pop(n = 0), character(0))
  expect_equal(x$list("names"), nms)
  expect_equal(unname(x$list("priorities")), pri)
  expect_equal(x$pop0(), "Preservation")
  expect_true(!length(x$pop0()))
  expect_equal(x$list("names"), nms[-1])
  expect_equal(unname(x$list("priorities")), pri[-1])

  x$decrease_key("soup")
  x$decrease_key("soup")
  nms <- c("baz", "Amy", "spren", "soup", "Ruin", "Bob", "foo")
  pri <- c(3, 4, 5, 5, 6.66, 7, 8)
  expect_equal(x$list("names"), nms)
  expect_equal(unname(x$list("priorities")), pri)
})
