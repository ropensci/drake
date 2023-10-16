drake_context("keras")

if (FALSE) {

test_with_dir("custom keras format", {
  skip_on_cran()
  skip_if_not_installed("keras")
  keras_model <- function() {
    model <- keras_model_sequential() %>%
      layer_conv_2d(
        filters = 32,
        kernel_size = c(3, 3),
        activation = "relu",
        input_shape = c(28, 28, 1)
      ) %>%
      layer_conv_2d(
        filters = 64,
        kernel_size = c(3, 3),
        activation = "relu"
      ) %>%
      layer_max_pooling_2d(pool_size = c(2, 2)) %>%
      layer_dropout(rate = 0.25) %>%
      layer_flatten() %>%
      layer_dense(units = 128, activation = "relu") %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 10, activation = "softmax")
    compile(
      model,
      loss = "categorical_crossentropy",
      optimizer = optimizer_adadelta(),
      metrics = c("accuracy")
    )
    model
  }
  plan <- drake_plan(x = target(keras_model(), format = "keras"))
  make(plan, packages = "keras")
  out <- readd(x)
  expect_true(inherits(out, "keras.engine.training.Model"))
  cache <- drake_cache()
  expect_true(
    inherits(
      cache$get_value(cache$get_hash("x")),
      "keras.engine.training.Model"
    )
  )
  ref <- cache$storr$get("x")
  expect_true(inherits(ref, "drake_format_keras"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
})

test_with_dir("keras + clustermq", {
  skip_on_cran()
  skip_if_not_installed("clustermq", minimum_version = "0.9.1")
  skip_if_not_installed("keras")
  skip_on_os("windows")
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
  options(clustermq.scheduler = "multicore")
  keras_model <- function() {
    model <- keras_model_sequential() %>%
      layer_conv_2d(
        filters = 32,
        kernel_size = c(3, 3),
        activation = "relu",
        input_shape = c(28, 28, 1)
      ) %>%
      layer_conv_2d(
        filters = 64,
        kernel_size = c(3, 3),
        activation = "relu"
      ) %>%
      layer_max_pooling_2d(pool_size = c(2, 2)) %>%
      layer_dropout(rate = 0.25) %>%
      layer_flatten() %>%
      layer_dense(units = 128, activation = "relu") %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 10, activation = "softmax")
    compile(
      model,
      loss = "categorical_crossentropy",
      optimizer = optimizer_adadelta(),
      metrics = c("accuracy")
    )
    model
  }
  plan <- drake_plan(x = target(keras_model(), format = "keras"))
  make(
    plan,
    packages = "keras",
    parallelism = "clustermq",
    caching = "main"
  )
  out <- readd(x)
  expect_true(inherits(out, "keras.engine.training.Model"))
  cache <- drake_cache()
  expect_true(
    inherits(
      cache$get_value(cache$get_hash("x")),
      "keras.engine.training.Model"
    )
  )
  ref <- cache$storr$get("x")
  expect_true(inherits(ref, "drake_format_keras"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
})

test_with_dir("keras + future", {
  skip_on_cran()
  skip_if_not_installed("future")
  skip_if_not_installed("keras")
  skip_on_os("windows")
  future::plan(future::sequential)
  keras_model <- function() {
    model <- keras_model_sequential() %>%
      layer_conv_2d(
        filters = 32,
        kernel_size = c(3, 3),
        activation = "relu",
        input_shape = c(28, 28, 1)
      ) %>%
      layer_conv_2d(
        filters = 64,
        kernel_size = c(3, 3),
        activation = "relu"
      ) %>%
      layer_max_pooling_2d(pool_size = c(2, 2)) %>%
      layer_dropout(rate = 0.25) %>%
      layer_flatten() %>%
      layer_dense(units = 128, activation = "relu") %>%
      layer_dropout(rate = 0.5) %>%
      layer_dense(units = 10, activation = "softmax")
    compile(
      model,
      loss = "categorical_crossentropy",
      optimizer = optimizer_adadelta(),
      metrics = c("accuracy")
    )
    model
  }
  plan <- drake_plan(x = target(keras_model(), format = "keras"))
  make(
    plan,
    packages = "keras",
    parallelism = "future",
    caching = "worker"
  )
  out <- readd(x)
  expect_true(inherits(out, "keras.engine.training.Model"))
  cache <- drake_cache()
  expect_true(
    inherits(
      cache$get_value(cache$get_hash("x")),
      "keras.engine.training.Model"
    )
  )
  ref <- cache$storr$get("x")
  expect_true(inherits(ref, "drake_format_keras"))
  expect_equal(length(ref), 1L)
  expect_true(nchar(ref) < 100)
  expect_false(is.list(ref))
})

}
