drake_context("message queue")

test_with_dir("custom message queue works", {
  q <- message_queue(tempfile())
  expect_true(file.exists(q$path))
  expect_true(file.exists(q$db))
  expect_true(file.exists(q$head))
  expect_true(is.character(q$lock) && length(q$lock) == 1)
  expect_true(q$empty())
  expect_equal(q$count(), 0)
  expect_equal(scan(q$head, quiet = TRUE), 1)
  null_df <- data.frame(
    title = character(0),
    message = character(0),
    stringsAsFactors = FALSE
  )
  expect_equal(q$pop(), null_df)
  expect_equal(q$pop(), null_df)
  expect_equal(q$list(), null_df)
  q$push(title = 1, message = 2)
  q$push(title = "74", message = "\"128\"")
  q$push(title = "71234", message = "My sentence is not long.")
  hash <- digest::digest(q$db, file = TRUE)
  expect_false(q$empty())
  expect_equal(q$count(), 3)
  expect_equal(scan(q$head, quiet = TRUE), 1)
  full_df <- data.frame(
    title = c(1, "74", "71234"),
    message = c(2, "\"128\"", "My sentence is not long."),
    stringsAsFactors = FALSE
  )
  expect_equal(q$list(), full_df)
  o <- q$pop(1)
  expect_false(q$empty())
  expect_equal(q$count(), 2)
  expect_equal(scan(q$head, quiet = TRUE), 2)
  expect_equal(hash, digest::digest(q$db, file = TRUE))
  expect_equal(q$list()$title, full_df[-1, "title"])
  expect_equal(q$list()$message, full_df[-1, "message"])
  out <- q$pop(-1)
  expect_equal(out$title, full_df[-1, "title"])
  expect_equal(out$message, full_df[-1, "message"])
  expect_true(q$empty())
  expect_equal(q$count(), 0)
  expect_equal(scan(q$head, quiet = TRUE), 4)
  expect_equal(q$list(), null_df)
  expect_equal(hash, digest::digest(q$db, file = TRUE))
  q$push(title = "new", message = "message")
  expect_false(q$empty())
  expect_equal(q$count(), 1)
  expect_equal(scan(q$head, quiet = TRUE), 4)
  one_df <- data.frame(
    title = "new",
    message = "message",
    stringsAsFactors = FALSE
  )
  expect_equal(q$list(), one_df)
  expect_true(file.exists(q$path))
  q$destroy()
  expect_false(file.exists(q$path))
})
