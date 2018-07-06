drake_context("generate")

test_with_dir("empty generative args", {
  x <- drake_plan(a = 1, b = FUNCTION())
  expect_equal(evaluate_plan(x), x)
  expect_equal(evaluate_wildcard_rules(x, rules = NULL), x)
  expect_equal(expand_plan(x), x)
})

test_with_dir("evaluate, expand, and gather", {
  df <- drake_plan(data = simulate(center = MU, scale = SIGMA))
  m0 <- evaluate_plan(df, wildcard = "NULL", values = 1:2)
  expect_equal(m0, df)
  m1 <- evaluate_plan(df, rules = list(nothing = 1:2), expand = FALSE)
  expect_equal(m1, df)

  x <- expand_plan(df, values = c("rep1", "rep2"))
  y <- tibble::tibble(
    target = c("data_rep1", "data_rep2"),
    command = rep("simulate(center = MU, scale = SIGMA)", 2)
  )
  expect_equal(x, y)

  x1 <- expand_plan(df, values = c("rep1", "rep2"), rename = TRUE)
  x2 <- expand_plan(df, values = c("rep1", "rep2"), rename = FALSE)
  y2 <- tibble::tibble(
    target = c("data", "data"),
    command = rep("simulate(center = MU, scale = SIGMA)", 2)
  )
  expect_equal(x1, y)
  expect_equal(x2, y2)

  x2 <- evaluate_plan(x, wildcard = "MU", values = 1:2)
  y <- tibble::tibble(
    target = c("data_rep1_1", "data_rep1_2", "data_rep2_1", "data_rep2_2"),
    command = c(
      "simulate(center = 1, scale = SIGMA)",
      "simulate(center = 2, scale = SIGMA)",
      "simulate(center = 1, scale = SIGMA)",
      "simulate(center = 2, scale = SIGMA)"
    )
  )
  expect_equal(x2, y)

  x3 <- evaluate_plan(x2, wildcard = "SIGMA", values = letters[1:2],
    expand = FALSE)
  y <- tibble(
    target = c("data_rep1_1", "data_rep1_2", "data_rep2_1", "data_rep2_2"),
    command = c(
      "simulate(center = 1, scale = a)",
      "simulate(center = 2, scale = b)",
      "simulate(center = 1, scale = a)",
      "simulate(center = 2, scale = b)"
    )
  )
  expect_equal(x3, y)

  x4 <- evaluate_plan(x, rules = list(MU = 1:2, SIGMA = c(0.1, 1)),
    expand = FALSE)
  y <- tibble(
    target = c("data_rep1", "data_rep2"),
    command = c(
      "simulate(center = 1, scale = 0.1)",
      "simulate(center = 2, scale = 1)"
    )
  )
  expect_equal(x4, y)

  x5 <- evaluate_plan(x, rules = list(MU = 1:2, SIGMA = c(0.1, 1, 10)))
  expect_equal(12, nrow(x5))
  expect_equal(12, length(unique(x5$target)))
  expect_equal(6, length(unique(x5$command)))

  x6 <- gather_plan(x)
  y <- tibble(
    target = "target",
    command = "list(data_rep1 = data_rep1, data_rep2 = data_rep2)"
  )
  expect_equal(x6, y)

  x7 <- gather_plan(x, target = "my_summaries", gather = "rbind")
  y <- tibble(
    target = "my_summaries",
    command = "rbind(data_rep1 = data_rep1, data_rep2 = data_rep2)"
  )
  expect_equal(x7, y)
})

test_with_dir("analyses and summaries", {
  datasets <- drake_plan(small = simulate(5), large = simulate(50))
  methods <- drake_plan(
    regression1 = reg1(dataset__),
    regression2 = reg2(dataset__)
  )
  analyses <- plan_analyses(methods, datasets = datasets)
  x <- tibble(
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
      "reg2(large)")
  )
  expect_equal(analyses, x)

  m2 <- drake_plan(regression1 = reg1(n), regression2 = reg2(n))
  expect_equal(plan_analyses(m2, datasets = datasets), m2)

  no_analyses <- drake_plan(
    summ = summary(dataset__),
    coef = coefficients(dataset__)
  )
  suppressWarnings(
    expect_error(
      plan_summaries(no_analyses, analyses, datasets)
    )
  )

  summary_types <- drake_plan(
    summ = summary(analysis__),
    coef = coefficients(analysis__)
  )
  results <- plan_summaries(summary_types, analyses, datasets, gather = NULL)
  x <- tibble(
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
    )
  )
  expect_equal(results, x)

  summary_types <- drake_plan(
    summ = summary(analysis__, dataset__),
    coef = coefficients(analysis__)
  )
  results <- plan_summaries(
    summary_types,
    analyses,
    datasets,
    gather = c("list", "rbind")
  )
  x <- tibble(
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
    )
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
    drake_plan(
      other = myother(dataset__)
    )
  )
  expect_warning(s <- plan_summaries(newtypes, analyses, datasets,
    gather = NULL))
  expect_equal(nrow(s), 8)
})

test_with_dir("reduce_plan()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  # Non-pairwise reduce
  x_plan <- evaluate_plan(
    drake_plan(x = VALUE),
    wildcard = "VALUE",
    values = 1:8
  )
  x <- reduce_plan(
    x_plan, target = "x_sum", pairwise = FALSE,
    begin = "", end = ""
  )
  x0 <- tibble::tibble(
    target = "x_sum",
    command = paste0(x_plan$target, collapse = " + ")
  )
  expect_equal(x, x0)
  make(rbind(x_plan, x), session_info = FALSE)
  expect_equal(readd(x_sum), sum(1:8))
  clean(destroy = TRUE)

  # Pairwise reduce even number of targets
  x <- reduce_plan(x_plan, target = "x_sum", pairwise = TRUE)
  x0 <- tibble(
    target = c(paste0("x_sum_", 1:6), "x_sum"),
    command = c(
      "x_1 + x_2", "x_3 + x_4", "x_5 + x_6", "x_7 + x_8",
      "x_sum_1 + x_sum_2", "x_sum_3 + x_sum_4",
      "x_sum_5 + x_sum_6"
    )
  )
  expect_equal(x, x0)
  x <- reduce_plan(
    x_plan, target = "x_sum", pairwise = FALSE,
    begin = "", end = ""
  )
  x0 <- tibble::tibble(
    target = "x_sum",
    command = paste0(x_plan$target, collapse = " + ")
  )
  expect_equal(x, x0)
  x <- reduce_plan(x_plan, target = "x_sum", pairwise = TRUE)
  x0 <- tibble(
    target = c(paste0("x_sum_", 1:6), "x_sum"),
    command = c(
      "x_1 + x_2", "x_3 + x_4", "x_5 + x_6", "x_7 + x_8",
      "x_sum_1 + x_sum_2", "x_sum_3 + x_sum_4",
      "x_sum_5 + x_sum_6"
    )
  )
  expect_equal(x, x0)
  make(rbind(x_plan, x), session_info = FALSE)
  expect_equal(readd(x_sum), sum(1:8))
  clean(destroy = TRUE)

  # Odd number of targets
  x_plan <- evaluate_plan(
    drake_plan(x = VALUE),
    wildcard = "VALUE",
    values = 1:9
  )
  x <- reduce_plan(x_plan, target = "x_sum", pairwise = TRUE)
  x0 <- tibble(
    target = c(paste0("x_sum_", 1:7), "x_sum"),
    command = c(
      "x_1 + x_2", "x_3 + x_4", "x_5 + x_6", "x_7 + x_8",
      "x_9 + x_sum_1",
      "x_sum_2 + x_sum_3", "x_sum_4 + x_sum_5",
      "x_sum_6 + x_sum_7"
    )
  )
  expect_equal(x, x0)
  make(rbind(x_plan, x), session_info = FALSE)
  expect_equal(readd(x_sum), sum(1:9))
  clean(destroy = TRUE)

  # Arbitrary function in reduction
  x_plan <- evaluate_plan(
    drake_plan(x = VALUE),
    wildcard = "VALUE",
    values = 1:8
  )
  fun <- function(x, y){
    x ^ 2 - 3 * y
  }
  x <- reduce_plan(x_plan, target = "x_sum", pairwise = TRUE,
    begin = "fun(", op = ", ", end = ")")
  x0 <- tibble(
    target = c(paste0("x_sum_", 1:6), "x_sum"),
    command = c(
      "fun(x_1, x_2)", "fun(x_3, x_4)", "fun(x_5, x_6)", "fun(x_7, x_8)",
      "fun(x_sum_1, x_sum_2)", "fun(x_sum_3, x_sum_4)",
      "fun(x_sum_5, x_sum_6)"
    )
  )
  expect_equal(x, x0)
  make(rbind(x_plan, x))
  out <- fun(
    fun(
      fun(1, 2),
      fun(3, 4)
    ),
    fun(
      fun(5, 6),
      fun(7, 8)
    )
  )
  expect_equal(readd(x_sum), out)
})

test_with_dir("non-expanded grid, issue 235", {
  rules_grid <- tibble::tibble(
    school_ =  c("schoolA", "schoolB", "schoolC"),
    funding_ = c("public", "public", "private")
  )
  rules_grid <- tidyr::crossing(
    rules_grid, cohort_ = c("2012", "2013", "2014", "2015"))
  rules_grid <- dplyr::filter(
    rules_grid, !(school_ == "schoolB" & cohort_ %in% c("2012", "2013")))

  plan <- drake_plan(
    credits = check_credit_hours("school_", "funding_", "cohort_"),
    students = check_students("school_", "funding_", "cohort_"),
    grads = check_graduations("school_", "funding_", "cohort_"),
    public_funds = check_public_funding("school_", "funding_", "cohort_"),
    strings_in_dots = "literals"
  )[c(rep(1, 4), rep(2, 2), rep(3, 4)), ]
  plan <- evaluate_plan(
    plan,
    rules = rules_grid,
    expand = FALSE,
    rename = TRUE
  )
  expect_equal(nrow(plan), 10)
  expect_equal(
    plan$target,
    c(
      "credits_schoolA_public_2012",
      "credits_schoolA_public_2013",
      "credits_schoolA_public_2014",
      "credits_schoolA_public_2015",
      "students_schoolB_public_2014",
      "students_schoolB_public_2015",
      "grads_schoolC_private_2012",
      "grads_schoolC_private_2013",
      "grads_schoolC_private_2014",
      "grads_schoolC_private_2015"
    )
  )
})

test_with_dir("evaluate_plan() and trace", {
  plan <- drake_plan(
    top = 3,
    data = simulate(center = MU, scale = SIGMA),
    mus = c(MU, x),
    simple = 1,
    sigmas = c(SIGMA, y),
    cheap = 2
  )

  x <- evaluate_plan(
    plan, trace = TRUE, wildcard = "MU", values = 1:2, expand = FALSE)
  y <- tibble::tibble(
    target = c(
      "top",
      "data",
      "mus",
      "simple",
      "sigmas",
      "cheap"
    ),
    command = c(
      3,
      "simulate(center = 1, scale = SIGMA)",
      "c(2, x)",
      1,
      "c(SIGMA, y)",
      2
    ),
    MU = as.character(c(NA, 1, 2, NA, NA, NA))
  )
  expect_equal(x, y)

  x <- evaluate_plan(
    plan, trace = TRUE, wildcard = "SIGMA", values = 1:2, expand = FALSE)
  y <- tibble::tibble(
    target = c(
      "top",
      "data",
      "mus",
      "simple",
      "sigmas",
      "cheap"
    ),
    command = c(
      3,
      "simulate(center = MU, scale = 1)",
      "c(MU, x)",
      1,
      "c(2, y)",
      2
    ),
    SIGMA = as.character(c(NA, 1, NA, NA, 2, NA))
  )
  expect_equal(x, y)

  x <- evaluate_plan(plan, trace = TRUE, wildcard = "MU", values = 1:2)
  y <- tibble::tibble(
    target = c(
      "top",
      "data_1",
      "data_2",
      "mus_1",
      "mus_2",
      "simple",
      "sigmas",
      "cheap"
    ),
    command = c(
      3,
      "simulate(center = 1, scale = SIGMA)",
      "simulate(center = 2, scale = SIGMA)",
      "c(1, x)",
      "c(2, x)",
      1,
      "c(SIGMA, y)",
      2
    ),
    MU = as.character(c(NA, 1, 2, 1, 2, NA, NA, NA))
  )
  expect_equal(x, y)

  x <- evaluate_plan(
    plan, trace = TRUE, rules = list(MU = 1:2, SIGMA = 3:4), expand = FALSE)
  y <- tibble::tibble(
    target = c(
      "top",
      "data",
      "mus",
      "simple",
      "sigmas",
      "cheap"
    ),
    command = c(
      3,
      "simulate(center = 1, scale = 3)",
      "c(2, x)",
      1,
      "c(4, y)",
      2
    ),
    MU = as.character(c(NA, 1, 2, NA, NA, NA)),
    SIGMA = as.character(c(NA, 3, NA, NA, 4, NA))
  )
  expect_equal(x, y)

  x <- evaluate_plan(plan, trace = TRUE, rules = list(MU = 1:2, SIGMA = 3:4))
  y <- tibble::tibble(
    target = c(
      "top",
      "data_1_3",
      "data_1_4",
      "data_2_3",
      "data_2_4",
      "mus_1",
      "mus_2",
      "simple",
      "sigmas_3",
      "sigmas_4",
      "cheap"
    ),
    command = c(
      3,
      "simulate(center = 1, scale = 3)",
      "simulate(center = 1, scale = 4)",
      "simulate(center = 2, scale = 3)",
      "simulate(center = 2, scale = 4)",
      "c(1, x)",
      "c(2, x)",
      1,
      "c(3, y)",
      "c(4, y)",
      2
    ),
    MU = as.character(c(NA, 1, 1, 2, 2, 1, 2, NA, NA, NA, NA)),
    SIGMA = as.character(c(NA, 3, 4, 3, 4, NA, NA, NA, 3, 4, NA))
  )
  expect_equal(x, y)
})
