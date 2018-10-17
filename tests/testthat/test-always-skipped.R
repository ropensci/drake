if (FALSE){

drake_context("always skipped")

test_with_dir("make() uses the worker column of the plan", {
  skip_on_cran() # CRAN gets whitelist tests only (check time restrictions).
  future::plan(future::sequential)
  envir <- new.env(parent = globalenv())
  load_mtcars_example(envir = envir)
  my_plan <- envir$my_plan
  my_plan$priority <- seq_len(nrow(my_plan))
  my_plan$worker <- 2
  my_plan$worker[grepl("small", my_plan$target)] <- 4
  make(my_plan, envir = envir, jobs = 4, verbose = 4,
       session_info = FALSE, pruning_strategy = "memory")
  expect_true(file_store("report.md") %in% cached())
  q1 <- txtq::txtq(file.path(".drake", "workers", "worker_1_ready"))
  q2 <- txtq::txtq(file.path(".drake", "workers", "worker_2_ready"))
  q3 <- txtq::txtq(file.path(".drake", "workers", "worker_3_ready"))
  q4 <- txtq::txtq(file.path(".drake", "workers", "worker_4_ready"))
  expect_false(any(q1$log() %in% my_plan$target))
  expect_false(any(q3$log() %in% my_plan$target))
  expect_false(any(grepl("small", q2$log()$title)))
  expect_true(any(grepl("small", q4$log()$title)))
  expect_false(length(intersect(q1$log()$title, my_plan$target)) > 1)
  expect_true(length(intersect(q2$log()$title, my_plan$target)) > 1)
  expect_false(length(intersect(q3$log()$title, my_plan$target)) > 1)
  expect_true(length(intersect(q4$log()$title, my_plan$target)) > 1)
})

# Always skipped due to the obtrusive error messages.
test_with_dir("failed mclapply workers terminate gracefully", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  plan <- drake_plan(
    a = stop(123),
    b = a + 1
  )
  make(plan, jobs = 2, session_info = FALSE, verbose = FALSE)
  expect_false(any(c("a", "b") %in% cached()))
  plan <- drake_plan(
    a = 123,
    b = a + 1
  )
  make(plan, jobs = 2, session_info = FALSE)
  expect_equal(readd(b), 124)
})

# Always skipped due to the obtrusive error messages.
test_with_dir("failed future_lapply workers terminate gracefully", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  plan <- drake_plan(
    a = stop(123),
    b = a + 1
  )
  future::plan(future::sequential)
  make(
    plan, jobs = 1, parallelism = "future_lapply",
    session_info = FALSE, verbose = FALSE
  )
  expect_false(any(c("a", "b") %in% cached()))
  plan <- drake_plan(
    a = 123,
    b = a + 1
  )
  make(plan, jobs = 2, session_info = FALSE)
  expect_equal(readd(b), 124)
})

test_with_dir("can keep going in parallel", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  plan <- drake_plan(
    a = stop(123),
    b = a + 1
  )
  make(
    plan, jobs = 2, session_info = FALSE, keep_going = TRUE, verbose = FALSE)
  expect_error(readd(a))
  expect_equal(readd(b), numeric(0))
})

test_with_dir("drake_debug()", {
  skip_on_cran()
  load_mtcars_example()
  my_plan$command[2] <- "simulate(48); stop(1234)"
  config <- drake_config(my_plan)
  expect_error(make(my_plan), regexp = "1234")
  expect_error(drake_debug(), regexp = "1234")
  out <- drake_debug(large, config)
  out <- drake_debug("large", config, verbose = "false", character_only = TRUE)
  expect_true(is.data.frame(out))
  my_plan$command <- lapply(
    X = as.list(my_plan$command),
    FUN = function(x){
      parse(text = x)[[1]]
    }
  )
  for (i in 1:2){
    clean(destroy = TRUE)
    load_mtcars_example()
    config <- make(my_plan)
    out <- drake_debug(small, config = config)
    expect_true(is.data.frame(out))
  }
})

test_with_dir("clustermq error messages get back to master", {
  plan <- drake_plan(a = stop(123))
  options(clustermq.scheduler = "multicore")
  for (caching in c("worker", "master")){
    expect_error(
      make(
        plan,
        parallelism = "clustermq",
        caching = "worker"
      ),
      regexp = "123"
    )
  }
})

}
