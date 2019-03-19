drake_context("pipe")

test_with_dir("%dp% really is a pipe", {
  skip_on_cran()
  out <- drake_plan(
    result = data %dp%
      task1() %dp%
      task2(data = my_data, ., x = .) %dp%
      task3()
  )
  exp <- drake_plan(
    result_2 = data,
    result_3 = task1(result_2),
    result_4 = task2(data = my_data, result_3, x = result_3),
    result = task3(result_4)
  )
  equivalent_plans(out, exp)
})

test_with_dir("%dp% understands symbols", {
  skip_on_cran()
  out <- drake_plan(
    result = data %dp%
      task1 %dp%
      task2
  )
  exp <- drake_plan(
    result_2 = data,
    result_3 = task1(result_2),
    result = task2(result_3)
  )
  equivalent_plans(out, exp)
})

test_with_dir("%dp% with other custom columns", {
  skip_on_cran()
  out <- drake_plan(
    x = 1,
    result = target(
      data %dp%
        task1() %dp%
        task2() %dp%
        task3(),
      timeout = 123
    ),
    other = target(
      data %dp%
        task1(x, .) %dp%
        task2(task3(.), .) %dp%
        task4(),
      timeout = 456
    ),
    y = 3
  )
  exp <- drake_plan(
    x = 1,
    result_2 = target(
      command = data,
      timeout = 123
    ),
    result_3 = target(
      command = task1(result_2),
      timeout = 123
    ),
    result_4 = target(
      command = task2(result_3),
      timeout = 123
    ),
    result = target(
      command = task3(result_4),
      timeout = 123
    ),
    other_2 = target(
      command = data,
      timeout = 456
    ),
    other_3 = target(
      command = task1(x, other_2),
      timeout = 456
    ),
    other_4 = target(
      command = task2(task3(other_3), other_3),
      timeout = 456
    ),
    other = target(
      command = task4(other_4),
      timeout = 456
    ),
    y = 3
  )
  equivalent_plans(out, exp)
})

test_with_dir("%dp% and transforms", {
  skip_on_cran()
  out <- drake_plan(
    x = 1,
    result = target(
      data %dp%
        task1() %dp%
        task2(x, .) %dp%
        task3(),
      transform = map(x = c(1, 2))
    ),
    y = 3
  )
  exp <- drake_plan(
    x = 1,
    result_1_2 = data,
    result_1_3 = task1(result_1_2),
    result_1_4 = task2(1, result_1_3),
    result_1 = task3(result_1_4),
    result_2_2 = data,
    result_2_3 = task1(result_2_2),
    result_2_4 = task2(2, result_2_3),
    result_2 = task3(result_2_4),
    y = 3
  )
  equivalent_plans(out, exp)
})

test_with_dir("%dp% does not mess with the trace", {
  skip_on_cran()
  out <- drake_plan(
    result = target(
      task1(data, analysis) %dp%
        task2() %dp%
        task3(),
      transform = map(analysis = c("bayes", "freq"))
    ),
    end = target(
      list(result),
      transform = combine(result)
    )
  )
  # nolint start
  exp <- drake_plan(
    result_.bayes._2 = task1(data, "bayes"),
    result_.bayes._3 = task2(result_.bayes._2),
    result_.bayes. = task3(result_.bayes._3),
    result_.freq._2 = task1(data, "freq"),
    result_.freq._3 = task2(result_.freq._2),
    result_.freq. = task3(result_.freq._3),
    end = list(result_.bayes., result_.freq.)
  )
  # nolint end
  equivalent_plans(out, exp)
})

test_with_dir("%dp% and a busy first call", {
  skip_on_cran()
  out <- drake_plan(
    x = (f(g(1)) + h(2)) %dp% # Need parens. So does magrittr.
      task_2() %dp%
      task_3()
  )
  exp <- drake_plan(
    x_2 = (f(g(1)) + h(2)),
    x_3 = task_2(x_2),
    x = task_3(x_3)
  )
  equivalent_plans(out, exp)
})

test_with_dir("%dp% and anonymous functions", {
  skip_on_cran()
  out <- drake_plan(
    x = data %dp% (function (x) {
        process_stuff(x)
      })
  )
  expect_equal(
    gsub(" |\n", "", safe_deparse(out$command[[2]])),
    "(function(x){process_stuff(x)})(x_2)"
  )
  expect_equal(nrow(out), 2L)
  expect_error(
    drake_plan(
      x = data %dp% function (x) {
        process_stuff(x)
      }
    ),
    regexp = "parenthesized"
  )
})
