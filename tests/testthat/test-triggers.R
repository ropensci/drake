drake_context("triggers")

test_with_dir("trigger() function works", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- 1
  y <- trigger(
    condition = 1 + 1,
    command = TRUE,
    depends = FALSE,
    file = today() == "Tuesday",
    change = sqrt(!!x)
  )
  z <- list(
    condition = quote(1 + 1),
    command = TRUE,
    depends = FALSE,
    file = FALSE,
    change = quote(sqrt(1))
  )
  expect_equal(y, z)
})

test_with_dir("can detect trigger deps", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  y <- 1
  today <- function(){
    "Monday"
  }
  plan <- drake_plan(
    x = target(
      command = 1 + 1,
      trigger = trigger(
        condition = sqrt(1 + 1) < y,
        change = today() == "Tuesday"
      )
    ),
    strings_in_dots = "literals"
  )
  config <- drake_config(
    plan, session_info = FALSE, cache = storr::storr_environment())
  expect_equal(sort(dependencies("x", config)), sort(c("today", "sqrt", "y")))
})

test_with_dir("empty triggers return logical", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_identical(depends_trigger("x", list(), list()), FALSE)
  expect_identical(command_trigger("x", list(), list()), FALSE)
  expect_identical(file_trigger("x", list(), list()), FALSE)
  expect_identical(change_trigger("x", list(), list()), FALSE)
})
