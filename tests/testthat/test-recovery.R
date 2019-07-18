drake_context("recovery")

test_with_dir("recovery (#945)", {
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
      },
      y = {
        x
        file.create("y")
      },
      z = {
        y
        file.create("z")
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
      "w"
    })
    config <- drake_config(plan)
    expect_equal(recoverable(config), character(0))
    make(plan, recover = TRUE, parallelism = parallelism, caching = caching)
    expect_equal(justbuilt(config), "w")
    expect_true(file.exists("w2"))
    unlink("w2")

    # change w back
    plan$command[[1]] <- quote({
      file.create("w")
      "w"
    })
    config <- drake_config(plan)
    expect_equal(recoverable(config), "w")
    make(plan, recover = TRUE, parallelism = parallelism, caching = caching)
    expect_false(file.exists("w"))
    expect_equal(justbuilt(config), "w")

    # Everything should be up to date now.
    make(plan, recover = TRUE, parallelism = parallelism, caching = caching)
    expect_false(file.exists("w"))
    expect_equal(justbuilt(config), character(0))
  }
  for (parallelism in c("loop", "clustermq", "future")) {
    for (caching in c("master", "worker")) {
      if (parallelism == "loop" && caching == "worker") {
        next
      }
      if (parallelism != "loop") {
        skip_on_cran()
      }
      test_recovery(parallelism, caching)
    }
  }
})
