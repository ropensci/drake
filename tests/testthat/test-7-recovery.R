drake_context("recovery")

test_with_dir("recovery (#945)", {
  skip_on_cran()
  test_recovery <- function(parallelism, caching) {
    clean(destroy = TRUE)
    plan <- drake_plan(
      w = {
        file.create("w")
        "w"
      },
      x = {
        w
        file.create("x")
        "x"
      },
      y = {
        x
        file.create("y")
        "y"
      },
      z = {
        y
        file.create("z")
        "z"
      }
    )

    # build everything
    config <- drake_config(plan)
    make(plan, recover = TRUE, parallelism = parallelism, caching = caching)
    expect_true(all(file.exists(plan$target)))
    unlink(plan$target)

    # change w
    plan$command[[1]] <- quote({
      file.create("w2")
      "w2"
    })
    config <- drake_config(plan)
    expect_equal(recoverable_impl(config), character(0))
    make(plan, recover = TRUE, parallelism = parallelism, caching = caching)
    expect_equal(sort(justbuilt(config)), sort(c("w", "x")))
    expect_true(file.exists("w2"))
    expect_true(file.exists("x"))
    unlink(c("w2", "x"))

    # change w back
    plan$command[[1]] <- quote({
      file.create("w")
      "w"
    })
    config <- drake_config(plan)
    expect_equal(recoverable_impl(config), "w")
    make(plan, recover = TRUE, parallelism = parallelism, caching = caching)
    expect_equal(readd(w, cache = config$cache), "w")
    expect_equal(readd(x, cache = config$cache), "x")
    expect_false(any(file.exists(c("w", "x"))))
    expect_equal(justbuilt(config), c("w", "x"))

    # Everything should be up to date now.
    make(plan, recover = TRUE, parallelism = parallelism, caching = caching)
    expect_false(file.exists("w"))
    expect_false(file.exists("x"))
    expect_equal(justbuilt(config), character(0))

    # Clean with garbage collection
    clean(garbage_collection = TRUE)

    # Can't recover `w`.
    plan$command[[1]] <- quote({
      file.create("w2")
      "w2"
    })
    config <- drake_config(plan)
    expect_equal(recoverable_impl(config), character(0))
  }

  skip_if_not_installed("future")
  options(clustermq.scheduler = "multicore")
  for (parallelism in c("loop", "future")) {
    for (caching in c("main", "worker")) {
      if (parallelism == "loop" && caching == "worker") {
        next
      }
      if (parallelism != "loop") {
        skip_on_cran()
        skip_on_os("windows")
      }
      test_recovery(parallelism, caching)
    }
  }
})

test_with_dir("rename a target", {
  skip_on_cran()
  plan <- drake_plan(
    raw_data = mtcars,
    data = {
      raw_data$cyl <- as.factor(raw_data$cyl)
      file.create("x")
      raw_data
    },
    summ = mean(data$mpg),
    fit = lm(mpg ~ wt + cyl, data)
  )
  plan$seed <- seq_len(nrow(plan))
  cache <- storr::storr_environment()
  make(plan, cache = cache, session_info = FALSE)
  clean()
  unlink("x")
  plan <- drake_plan(
    raw_data = mtcars,
    data = {
      raw_data$cyl <- as.factor(raw_data$cyl)
      file.create("x")
      raw_data
    },
    summ = mean(data$mpg),
    fit = lm(mpg ~ wt + cyl, data)
  )
  plan$seed <- seq_len(nrow(plan))
  make(plan, recover = TRUE, cache = cache, session_info = FALSE)
  expect_false(file.exists("x"))
  config <- drake_config(plan, cache = cache)
  expect_equal(outdated_impl(config), character(0))
})

test_with_dir("recovery with a non-standard trigger", {
  skip_on_cran()
  plan <- drake_plan(
    x = target({
      file.create("x")
      "x"
    },
    trigger = trigger(change = "a"))
  )
  make(plan, recover = TRUE)
  plan <- drake_plan(
    x = target({
      file.create("x")
      "y"
    },
    trigger = trigger(change = "b"))
  )
  make(plan, recover = TRUE)
  plan <- drake_plan(
    x = target({
      file.create("x")
      "x"
    },
    trigger = trigger(change = "a"))
  )
  unlink("x")
  make(plan, recover = TRUE)
  expect_false(file.exists("x"))
  expect_equal(readd(x), "x")
})

test_with_dir("recoverability can be disabled", {
  skip_on_cran()
  plan <- drake_plan(
    x = target(file.create("x"))
  )
  make(plan, recoverable = FALSE)
  plan <- drake_plan(
    x = target(file.create("y"))
  )
  make(plan, recover = TRUE)
  plan <- drake_plan(
    x = target(file.create("x"))
  )
  unlink("x")
  make(plan, recover = TRUE)
  expect_true(file.exists("x"))
})
