drake_context("pipe")

test_with_dir("%dp% really is a pipe", {
  out <- drake_plan(
    result = data %dp%
      task1() %dp%
      task2(data = my_data, ., x = .) %dp%
      task3()
  )
  exp <- drake_plan(
    result.1 = data,
    result.2 = task1(result.1),
    result.3 = task2(data = my_data, result.2, x = result.2),
    result = task3(result.3)
  )
  equivalent_plans(out, exp)
})

test_with_dir("%dp% understands symbols", {
  out <- drake_plan(
    result = data %dp%
      task1 %dp%
      task2
  )
  exp <- drake_plan(
    result.1 = data,
    result.2 = task1(result.1),
    result = task2(result.2)
  )
  equivalent_plans(out, exp)
})

test_with_dir("%dp% with other custom columns", {
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
    result.1 = target(
      command = data,
      timeout = 123
    ),
    result.2 = target(
      command = task1(result.1),
      timeout = 123
    ),
    result.3 = target(
      command = task2(result.2),
      timeout = 123
    ),
    result = target(
      command = task3(result.3),
      timeout = 123
    ),
    other.1 = target(
      command = data,
      timeout = 456
    ),
    other.2 = target(
      command = task1(x, other.1),
      timeout = 456
    ),
    other.3 = target(
      command = task2(task3(other.2), other.2),
      timeout = 456
    ),
    other = target(
      command = task4(other.3),
      timeout = 456
    ),
    y = 3
  )
  equivalent_plans(out, exp)
})

test_with_dir("%dp% and transforms", {
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
    result_1.1 = data,
    result_1.2 = task1(result_1.1),
    result_1.3 = task2(1, result_1.2),
    result_1 = task3(result_1.3),
    result_2.1 = data,
    result_2.2 = task1(result_2.1),
    result_2.3 = task2(2, result_2.2),
    result_2 = task3(result_2.3),
    y = 3
  )
  equivalent_plans(out, exp)
})

test_with_dir("%dp% does not mess with the trace", {
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
    result_.bayes..1 = task1(data, "bayes"),
    result_.bayes..2 = task2(result_.bayes..1),
    result_.bayes. = task3(result_.bayes..2),
    result_.freq..1 = task1(data, "freq"),
    result_.freq..2 = task2(result_.freq..1),
    result_.freq. = task3(result_.freq..2),
    end = list(result_.bayes., result_.freq.)
  )
  # nolint end
  equivalent_plans(out, exp)
})

test_with_dir("%dp% and a busy first call", {
  out <- drake_plan(
    x = (f(g(1)) + h(2)) %dp% # Need parens. So does magrittr.
      task_2() %dp%
      task_3()
  )
  exp <- drake_plan(
    x.1 = (f(g(1)) + h(2)),
    x.2 = task_2(x.1),
    x = task_3(x.2)
  )
  equivalent_plans(out, exp)
})

test_with_dir("%dp% and anonymous functions", {
  out <- drake_plan(
    x = data %dp% (function (x) {
        process_stuff(x)
      })
  )
  expect_equal(
    gsub(" |\n", "", safe_deparse(out$command[[2]])),
    "(function(x){process_stuff(x)})(x.1)"
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
