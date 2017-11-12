drake_context("generate")

test_with_dir("empty generative args", {
  x <- workplan(a = 1, b = FUNCTION())
  expect_equal(evaluate_plan(x), x)
  expect_equal(evaluations(x), x)
  expect_equal(expand_plan(x), x)
})

test_with_dir("evaluate, expand, and gather", {
  df <- workplan(data = simulate(center = MU, scale = SIGMA))
  m0 <- evaluate_plan(df, wildcard = "NULL", values = 1:2)
  expect_equal(m0, df)
  m1 <- evaluate_plan(df, rules = list(nothing = 1:2), expand = FALSE)
  expect_equal(m1, df)

  x <- expand_plan(df, values = c("rep1", "rep2"))
  y <- data.frame(
    target = c("data_rep1", "data_rep2"),
    command = rep("simulate(center = MU, scale = SIGMA)", 2),
    stringsAsFactors = FALSE
  )
  expect_equal(x, y)

  x2 <- evaluate_plan(x, wildcard = "MU", values = 1:2)
  y <- data.frame(
    target = c("data_rep1_1", "data_rep1_2", "data_rep2_1", "data_rep2_2"),
    command = c(
      "simulate(center = 1, scale = SIGMA)",
      "simulate(center = 2, scale = SIGMA)",
      "simulate(center = 1, scale = SIGMA)",
      "simulate(center = 2, scale = SIGMA)"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(x2, y)

  x3 <- evaluate_plan(x2, wildcard = "SIGMA", values = letters[1:2],
    expand = FALSE)
  y <- data.frame(
    target = c("data_rep1_1", "data_rep1_2", "data_rep2_1", "data_rep2_2"),
    command = c(
      "simulate(center = 1, scale = a)",
      "simulate(center = 2, scale = b)",
      "simulate(center = 1, scale = a)",
      "simulate(center = 2, scale = b)"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(x3, y)

  x4 <- evaluate_plan(x, rules = list(MU = 1:2, SIGMA = c(0.1, 1)),
    expand = FALSE)
  y <- data.frame(
    target = c("data_rep1", "data_rep2"),
    command = c(
      "simulate(center = 1, scale = 0.1)",
      "simulate(center = 2, scale = 1)"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(x4, y)

  x5 <- evaluate_plan(x, rules = list(MU = 1:2, SIGMA = c(0.1, 1, 10)))
  expect_equal(12, nrow(x5))
  expect_equal(12, length(unique(x5$target)))
  expect_equal(6, length(unique(x5$command)))

  x6 <- gather_plan(x)
  y <- data.frame(
    target = "target",
    command = "list(data_rep1 = data_rep1, data_rep2 = data_rep2)",
    stringsAsFactors = F
  )
  expect_equal(x6, y)

  x7 <- gather_plan(x, target = "my_summaries", gather = "rbind")
  y <- data.frame(
    target = "my_summaries",
    command = "rbind(data_rep1 = data_rep1, data_rep2 = data_rep2)",
    stringsAsFactors = F
  )
  expect_equal(x7, y)
})

test_with_dir("analyses and summaries", {
  datasets <- workplan(small = simulate(5), large = simulate(50))
  methods <- workplan(
    regression1 = reg1(dataset__),
    regression2 = reg2(dataset__)
  )
  analyses <- plan_analyses(methods, data = datasets)
  x <- data.frame(
    target = c(
      "regression1_small",
      "regression1_large",
      "regression2_small",
      "regression2_large"
    ),
    command = c(
      "reg1(small)",
      "reg1(large)",
      "reg2(small)",
      "reg2(large)"),
    stringsAsFactors = F
  )
  expect_equal(analyses, x)

  m2 <- workplan(regression1 = reg1(n), regression2 = reg2(n))
  expect_equal(plan_analyses(m2, data = datasets), m2)

  no_analyses <- workplan(
    summ = summary(dataset__),
    coef = coefficients(dataset__)
  )
  suppressWarnings(
    expect_error(
      plan_summaries(no_analyses, analyses, datasets)
    )
  )

  summary_types <- workplan(
    summ = summary(analysis__),
    coef = coefficients(analysis__)
  )
  results <- plan_summaries(summary_types, analyses, datasets, gather = NULL)
  x <- data.frame(
    target = c(
      "summ_regression1_small",
      "summ_regression1_large",
      "summ_regression2_small",
      "summ_regression2_large",
      "coef_regression1_small",
      "coef_regression1_large",
      "coef_regression2_small",
      "coef_regression2_large"
    ),
    command = c(
      "summary(regression1_small)",
      "summary(regression1_large)",
      "summary(regression2_small)",
      "summary(regression2_large)",
      "coefficients(regression1_small)",
      "coefficients(regression1_large)",
      "coefficients(regression2_small)",
      "coefficients(regression2_large)"
    ),
    stringsAsFactors = FALSE
  )
  expect_equal(results, x)

  summary_types <- workplan(
    summ = summary(analysis__, dataset__),
    coef = coefficients(analysis__)
  )
  results <- plan_summaries(
    summary_types,
    analyses,
    datasets,
    gather = c("list", "rbind")
  )
  x <- data.frame(
    target = c(
      "summ_regression1_small",
      "summ_regression1_large",
      "summ_regression2_small",
      "summ_regression2_large",
      "coef_regression1_small",
      "coef_regression1_large",
      "coef_regression2_small",
      "coef_regression2_large"
    ),
    command = c(
      "summary(regression1_small, small)",
      "summary(regression1_large, large)",
      "summary(regression2_small, small)",
      "summary(regression2_large, large)",
      "coefficients(regression1_small)",
      "coefficients(regression1_large)",
      "coefficients(regression2_small)",
      "coefficients(regression2_large)"
    ),
    stringsAsFactors = FALSE
  )
  y <- results[-1:-2, ]
  row.names(x) <- row.names(y) <- NULL
  expect_equal(x, y)
  expect_true(grepl("^rbind\\(coef", results$command[1]))
  expect_true(grepl("^list\\(summ", results$command[2]))

  results <- plan_summaries(summary_types, analyses, datasets)
  expect_true(all(grepl("^list\\(", results$command[1:2])))

  results <- plan_summaries(
    summary_types, analyses, datasets, gather = "my_bind")
  expect_true(all(grepl("^my_bind\\(", results$command[1:2])))

  expect_error(
    nope <- plan_summaries(
      summary_types,
      analyses,
      datasets,
      gather = rep("list", 37)
    )
  )

  newtypes <- rbind(
    summary_types,
    workplan(
      other = myother(dataset__)
    )
  )
  expect_warning(s <- plan_summaries(newtypes, analyses, datasets,
    gather = NULL))
  expect_equal(nrow(s), 8)
})
