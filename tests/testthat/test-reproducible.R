context("reproducible random numbers")

test_that("Objects are reproducible", {
  dclean()

  data <- plan(
    x = runif(20),
    y = runif(20),
    z = rnorm(20)
    )
  make(data, verbose = FALSE)
  old_x <- x
  old_y <- y
  old_z <- z

  # Oh no, I've accidentally deleted some data that needs to be reproducible
  clean(x, y, z)
  make(data, verbose = FALSE)
  expect_identical(x, old_x)
  expect_identical(y, old_y)
  expect_identical(z, old_z)

  dclean()
})

test_that("Objects are distinct", {
  dclean()

  data <- plan(
    x = runif(20)
    )
  data_exp <- expand(data, values = c("a", "b"))
  make(data_exp, verbose = FALSE)

  expect_false(identical(x_a, x_b))

  dclean()
})

test_that("Sequential objects reproduce correctly", {
  dclean()

  first_stage <- plan(
    x = runif(20)
    )
  methods <- plan(
    xbar = mean(..dataset..),  #nolint
    y = sample(..dataset.., 5)  #nolint
    )
  second_stage <- analyses(
    methods,
    data = first_stage
    )
  make(rbind(first_stage, second_stage), verbose = FALSE)
  old_x <- x
  old_xbax_x <- xbar_x
  old_y_y <- y_x

  # How do I keep deleting my data like this? So clumsy.
  clean(x, xbar_x, y_x)

  make(rbind(first_stage, second_stage), verbose = FALSE)
  expect_identical(old_x, x)
  expect_identical(old_xbax_x, xbar_x)
  expect_identical(old_y_y, y_x)

  dclean()
})
