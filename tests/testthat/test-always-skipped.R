if (FALSE){

drake_context("always skipped")

test_with_dir("make() uses the worker column of the plan", {
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

}
