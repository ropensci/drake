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
  y <- tibble::tibble(
    target = c("data_rep1_1", "data_rep1_2", "data_rep2_1", "data_rep2_2"),
    command = c(
      "simulate(center = 1, scale = a)",
      "simulate(center = 2, scale = b)",
      "simulate(center = 1, scale = a)",
      "simulate(center = 2, scale = b)"
    )
  )
  expect_equal(x3, y)

  x3a <- evaluate_plan(x2, wildcard = "SIGMA", values = letters[1:2],
                      expand = FALSE, rename = TRUE)
  y <- tibble::tibble(
    target = c(
      "data_rep1_1_a", "data_rep1_2_b", "data_rep2_1_a", "data_rep2_2_b"),
    command = c(
      "simulate(center = 1, scale = a)",
      "simulate(center = 2, scale = b)",
      "simulate(center = 1, scale = a)",
      "simulate(center = 2, scale = b)"
    )
  )
  expect_equal(x3a, y)

  x4 <- evaluate_plan(x, rules = list(MU = 1:2, SIGMA = c(0.1, 1)),
    expand = FALSE)
  y <- tibble::tibble(
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
  y <- tibble::tibble(
    target = "target",
    command = "list(data_rep1 = data_rep1, data_rep2 = data_rep2)"
  )
  expect_equal(x6, y)

  x7 <- gather_plan(x, target = "my_summaries", gather = "rbind")
  y <- tibble::tibble(
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
    MU = as.character(c(NA, 1, 2, NA, NA, NA)),
    MU_from = as.character(c(NA, "data", "mus", NA, NA, NA))
  )
  expect_equal(x, y)
  expect_equal(sort(attr(x, "wildcards")), sort(c("MU", "MU_from")))
  expect_silent(assert_standard_columns(list(plan = x)))

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
    SIGMA = as.character(c(NA, 1, NA, NA, 2, NA)),
    SIGMA_from = as.character(c(NA, "data", NA, NA, "sigmas", NA))
  )
  expect_equal(x, y)
  expect_equal(attr(x, "wildcards"), sort(c("SIGMA", "SIGMA_from")))

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
    MU = as.character(c(NA, 1, 2, 1, 2, NA, NA, NA)),
    MU_from = as.character(c(NA, "data", "data", "mus", "mus", NA, NA, NA))
  )
  expect_equal(x, y)
  expect_equal(sort(attr(x, "wildcards")), sort(c("MU", "MU_from")))

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
    MU_from = as.character(c(NA, "data", "mus", NA, NA, NA)),
    SIGMA = as.character(c(NA, 3, NA, NA, 4, NA)),
    SIGMA_from = as.character(c(NA, "data", NA, NA, "sigmas", NA))
  )
  expect_equal(x, y)
  expect_equal(
    sort(attr(x, "wildcards")),
    sort(c("MU", "MU_from", "SIGMA", "SIGMA_from"))
  )

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
    MU_from = as.character(
      c(NA, rep("data", 4), rep("mus", 2), NA, NA, NA, NA)
    ),
    SIGMA = as.character(c(NA, 3, 4, 3, 4, NA, NA, NA, 3, 4, NA)),
    SIGMA_from = as.character(
      c(
        NA, rep(c("data_1", "data_2"), each = 2),
        NA, NA, NA, rep("sigmas", 2), NA
      )
    )
  )
  expect_equal(x, y)
  expect_equal(
    sort(attr(x, "wildcards")),
    sort(c("MU", "MU_from", "SIGMA", "SIGMA_from"))
  )
})

test_with_dir("make() with wildcard columns", {
  plan <- evaluate_plan(
    drake_plan(x = rnorm(n__)),
    wildcard = "n__",
    values = 1:2,
    trace = TRUE
  )
  expect_equal(nrow(plan), 2)
  for (col in c("n__", "n___from")){
    expect_true(col %in% colnames(plan))
  }
  con <- make(plan, cache = storr::storr_environment(), session_info = FALSE)
  expect_true(all(plan$target %in% cached(cache = con$cache)))
  expect_identical(con$plan, plan)
  expect_equal(sort(attr(plan, "wildcards")), sort(c("n__", "n___from")))
  expect_equal(sort(attr(con$plan, "wildcards")), sort(c("n__", "n___from")))
})

test_with_dir("gather_by()", {
  skip_on_cran()
  plan <- evaluate_plan(
    drake_plan(x = rnorm(m__), y = rexp(n__), z = 10),
    rules = list(
      m__ = 1:2,
      n__ = c("a", "b")
    ),
    trace = TRUE
  )
  x <- gather_by(plan, n___from, prefix = "xyz", gather = "c")
  y <- tibble::tibble(
    target = "xyz_y",
    command = c("c(y_a = y_a, y_b = y_b)"),
    m__ = as.character(NA),
    m___from = as.character(NA),
    n__ = NA,
    n___from = "y"
  )
  expect_equal(x, bind_plans(plan, y))
  x <- gather_by(plan, m__, n__, prefix = "xyz", gather = "c")
  y <- tibble::tibble(
    target = c("xyz_1_NA", "xyz_2_NA", "xyz_NA_a", "xyz_NA_b"),
    command = c(
      "c(x_1 = x_1)",
      "c(x_2 = x_2)",
      "c(y_a = y_a)",
      "c(y_b = y_b)"
    ),
    m__ = as.character(c(1, 2, NA, NA)),
    m___from = as.character(NA),
    n__ = c(NA, NA, "a", "b"),
    n___from = as.character(NA)
  )
  expect_equal(x, bind_plans(plan, y))
})

test_with_dir("reduce_by()", {
  skip_on_cran()
  plan <- evaluate_plan(
    drake_plan(x = rnorm(m__), y = rexp(n__), z = 10),
    rules = list(
      m__ = 1:4,
      n__ = c("a", "b")
    ),
    trace = TRUE
  )
  x <- reduce_by(
    plan, m___from, prefix = "xyz", op = ", ", begin = "c(", end = ")"
  )
  y <- tibble::tibble(
    target = c("xyz_1_x", "xyz_2_x", "xyz_x"),
    command = c("c(x_1, x_2)", "c(x_3, x_4)", "c(xyz_1, xyz_2)"),
    m__ = as.character(NA),
    m___from = rep("x", 3),
    n__ = as.character(NA),
    n___from = as.character(NA)
  )
  expect_equal(x, bind_plans(plan, y))
  x <- reduce_by(
    plan, m___from, prefix = "xyz", op = ", ", begin = "c(", end = ")",
    pairwise = FALSE
  )
  y <- tibble::tibble(
    target = "xyz_x",
    command = "c(c(c(x_1, x_2), x_3), x_4)",
    m__ = as.character(NA),
    m___from = "x",
    n__ = as.character(NA),
    n___from = as.character(NA)
  )
  expect_equal(x, bind_plans(plan, y))
  x <- reduce_by(plan, m___from, n___from)
  y <- tibble::tibble(
    target = c(
      "target_1_x_NA",
      "target_2_x_NA",
      "target_x_NA",
      "target_NA_y"
    ),
    command = c(
      "x_1 + x_2",
      "x_3 + x_4",
      "target_1 + target_2",
      "y_a + y_b"
    ),
    m__ = as.character(NA),
    m___from = c(rep("x", 3), NA),
    n__ = as.character(NA),
    n___from = c(rep(NA, 3), "y")
  )
  expect_equal(x, bind_plans(plan, y))
  x <- reduce_by(plan, m___from, n___from, pairwise = FALSE)
  y <- tibble::tibble(
    target = c(
      "target_x_NA",
      "target_NA_y"
    ),
    command = c(
      "x_1 + x_2 + x_3 + x_4",
      "y_a + y_b"
    ),
    m__ = as.character(NA),
    m___from = c("x", NA),
    n__ = as.character(NA),
    n___from = c(NA, "y")
  )
  expect_equal(x, bind_plans(plan, y))
})
