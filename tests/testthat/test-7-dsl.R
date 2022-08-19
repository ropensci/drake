drake_context("dsl")

test_with_dir("nothing to transform", {
  skip_on_cran()
  exp <- drake_plan(a = 1)
  out <- transform_plan(exp)
  equivalent_plans(out, exp)
})

test_with_dir("empty transforms", {
  skip_on_cran()
  expect_error(
    out <- drake_plan(
      a = target(x, transform = cross()),
      b = target(y, transform = combine()),
      c = target(z, transform = map())
    ),
    regexp = "grouping variable"
  )
  expect_error(
    out <- drake_plan(a = target(x, transform = cross())),
    regexp = "grouping variable"
  )
  expect_error(
    out <- drake_plan(b = target(y, transform = combine())),
    regexp = "grouping variable"
  )
  expect_error(
    out <- drake_plan(c = target(z, transform = map())),
    regexp = "grouping variable"
  )
  x_vals <- NULL
  expect_error(
    out <- drake_plan(a = target(x, transform = map(x = !!x_vals))),
    regexp = "grouping variable"
  )
})

test_with_dir("1 grouping level", {
  out <- drake_plan(
    a = target(x, transform = cross(x = 1)),
    b = target(a, transform = map(a)),
    c = target(b, transform = combine(b))
  )
  exp <- drake_plan(
    a_1 = 1,
    b_a_1 = a_1,
    c = list(b_a_1)
  )
  equivalent_plans(out, exp)
})

test_with_dir("empty grouping levels", {
  out <- drake_plan(x = target(y, transform = map(y = c(z, NULL))))
  exp <- weak_tibble(
    target = c("x_z", "x_NULL"),
    command = c("z", "expression(NULL)")
  )
  equivalent_plans(out, exp)
})

test_with_dir("bad transform", {
  skip_on_cran()
  expect_error(
    drake_plan(x = target(1, transform = 132)),
    regexp = "invalid transform"
  )
})

test_with_dir("simple expansion", {
  out <- drake_plan(a = target(1 + 1, transform = cross(x = c(1, 2))))
  exp <- weak_tibble(
    target = c("a_1", "a_2"),
    command = rep("1 + 1", 2)
  )
  equivalent_plans(out, exp)
})

test_with_dir("replicates", {
  skip_on_cran()
  out <- drake_plan(
    trace = TRUE,
    a = target(x, transform = map(x = c(1, 1))),
    b = target(f(a), transform = map(a))
  )
  exp <- drake_plan(
    a_1 = target(
      command = 1,
      x = "1",
      a = "a_1"
    ),
    a_1_2 = target(
      command = 1,
      x = "1",
      a = "a_1_2"
    ),
    b_a_1 = target(
      command = f(a_1),
      x = "1",
      a = "a_1",
      b = "b_a_1"
    ),
    b_a_1_2 = target(
      command = f(a_1_2),
      x = "1",
      a = "a_1_2",
      b = "b_a_1_2"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("single tag_in", {
  out <- drake_plan(
    x = target(
      y,
      transform = cross(
        x = c(1, 2),
        .tag_in = single
      )
    ),
    trace = TRUE
  )
  exp <- drake_plan(
    x_1 = target(
      command = y,
      x = "x_1",
      single = "x"
    ),
    x_2 = target(
      command = y,
      x = "x_2",
      single = "x"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("multiple tag_in", {
  skip_on_cran()
  out <- drake_plan(
    x = target(
      y,
      transform = cross(
        x = c(1, 2),
        .tag_in = c(one, second)
      )
    ),
    trace = TRUE
  )
  exp <- drake_plan(
    x_1 = target(
      command = y,
      x = "x_1",
      one = "x",
      second = "x"
    ),
    x_2 = target(
      command = y,
      x = "x_2",
      one = "x",
      second = "x"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("single tag_out", {
  skip_on_cran()
  out <- drake_plan(
    x = target(
      y,
      transform = cross(
        x = c(1, 2),
        .tag_out = single
      )
    ),
    trace = TRUE
  )
  exp <- drake_plan(
    x_1 = target(
      command = y,
      x = "x_1",
      single = "x_1"
    ),
    x_2 = target(
      command = y,
      x = "x_2",
      single = "x_2"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("multiple tag_out", {
  skip_on_cran()
  out <- drake_plan(
    x = target(
      y,
      transform = cross(
        x = c(1, 2),
        .tag_out = c(one, second)
      )
    ),
    trace = TRUE
  )
  exp <- drake_plan(
    x_1 = target(
      command = y,
      x = "x_1",
      one = "x_1",
      second = "x_1"
    ),
    x_2 = target(
      command = y,
      x = "x_2",
      one = "x_2",
      second = "x_2"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("simple map", {
  out <- drake_plan(a = target(1 + 1, transform = map(x = c(1, 2))))
  exp <- weak_tibble(
    target = c("a_1", "a_2"),
    command = rep("1 + 1", 2)
  )
  equivalent_plans(out, exp)
})

test_with_dir("simple map with 2 factors", {
  out <- drake_plan(
    a = target(1 + 1, transform = map(x = c(1, 2), y = c(3, 4)))
  )
  exp <- weak_tibble(
    target = c("a_1_3", "a_2_4"),
    command = rep("1 + 1", 2)
  )
  equivalent_plans(out, exp)
})

test_with_dir("all new crossings", {
  out <- drake_plan(
    analysis = target(
      analyze_data(source),
      transform = cross(source = c(source1, source2))
    )
  )
  exp <- drake_plan(
    analysis_source1 = analyze_data(source1),
    analysis_source2 = analyze_data(source2)
  )
  equivalent_plans(out, exp)
})

test_with_dir("1 new map", {
  skip_on_cran()
  out <- drake_plan(
    analysis = target(
      analyze_data(source),
      transform = map(source = c(source1, source2))
    )
  )
  exp <- drake_plan(
    analysis_source1 = analyze_data(source1),
    analysis_source2 = analyze_data(source2)
  )
  equivalent_plans(out, exp)
})

test_with_dir("2 new maps", {
  out <- drake_plan(
    analysis = target(
      analyze_data(source, set),
      transform = map(source = c(source1, source2), set = c(set1, set2))
    )
  )
  exp <- drake_plan(
    analysis_source1_set1 = analyze_data(source1, set1),
    analysis_source2_set2 = analyze_data(source2, set2)
  )
  equivalent_plans(out, exp)
})

test_with_dir("groups and command symbols are undefined", {
  skip_on_cran()
  expect_error(
    out <- drake_plan(
      small = simulate(48),
      large = simulate(64),
      lots = target(nobody(home), transform = cross(a, b)),
      mots = target(everyone(out), transform = map(c, d)),
      winners = target(min(nobodyhome), transform = combine(data))
    ),
    regexp = "grouping variable"
  )
})

test_with_dir("command symbols are for combine() but the plan has them", {
  skip_on_cran()
  out <- drake_plan(
    data = target(x, transform = map(x = c(1, 2))),
    nope = target(x, transform = map(x = c(1, 2))),
    winners = target(min(data, nope), transform = combine(data))
  )
  exp <- drake_plan(
    data_1 = 1,
    data_2 = 2,
    nope_1 = 1,
    nope_2 = 2,
    winners = min(data_1, data_2, nope)
  )
  equivalent_plans(out, exp)
})

test_with_dir("combine different groups together", {
  skip_on_cran()
  out <- drake_plan(
    data_group1 = target(
      sim_data(mean = x, sd = y),
      transform = map(x = c(1, 2), y = c(3, 4))
    ),
    data_group2 = target(
      pull_data(url),
      transform = map(url = c("example1.com", "example2.com"))
    ),
    larger = target(
      bind_rows(data_group1, data_group2),
      transform = combine(
        data_group1,
        data_group2
      )
    )
  )
  exp <- drake_plan(
    data_group1_1_3 = sim_data(mean = 1, sd = 3),
    data_group1_2_4 = sim_data(mean = 2, sd = 4),
    data_group2_example1.com = pull_data("example1.com"),
    data_group2_example2.com = pull_data("example2.com"),
    larger = bind_rows(
      data_group1_1_3, data_group1_2_4,
      data_group2_example1.com, data_group2_example2.com # nolint
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("multiple groups and multiple splits", {
  out <- drake_plan(
    data_group1 = target(
      sim(mean = x, sd = y),
      transform = cross(x = c(1, 2), y = c(3, 4))
    ),
    data_group2 = target(
      pull(mean = x, sd = y),
      transform = cross(x = c(1, 2), y = c(3, 4))
    ),
    larger = target(
      bind_rows(data_group1, data_group2),
      transform = combine(
        data_group1,
        data_group2,
        .by = c(x, y)
      )
    )
  )
  exp <- drake_plan(
    data_group1_1_3 = sim(mean = 1, sd = 3),
    data_group1_2_3 = sim(mean = 2, sd = 3),
    data_group1_1_4 = sim(mean = 1, sd = 4),
    data_group1_2_4 = sim(mean = 2, sd = 4),
    data_group2_1_3 = pull(mean = 1, sd = 3),
    data_group2_2_3 = pull(mean = 2, sd = 3),
    data_group2_1_4 = pull(mean = 1, sd = 4),
    data_group2_2_4 = pull(mean = 2, sd = 4),
    larger_1_3 = bind_rows(data_group1_1_3, data_group2_1_3),
    larger_2_3 = bind_rows(data_group1_2_3, data_group2_2_3),
    larger_1_4 = bind_rows(data_group1_1_4, data_group2_1_4),
    larger_2_4 = bind_rows(data_group1_2_4, data_group2_2_4)
  )
  equivalent_plans(out, exp)
})

test_with_dir("dsl with different types", {
  plan <- drake_plan(
    a = target(1 + 1, transform = cross(x = c(1, 2))),
    transform = FALSE
  )
  plan$command <- list(quote(1 + 1))
  plan <- transform_plan(
    plan,
    envir = environment(),
    trace = FALSE,
    max_expand = NULL
  )
  plan$command <- unlist(lapply(plan$command, safe_deparse))
  expect_equal(sort(plan$target), sort(c("a_1", "a_2")))
  expect_equal(plan$command, rep("1 + 1", 2))
})

test_with_dir("dsl with a version of the mtcars plan", {
  out <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg = target(
      reg_fun(data),
      transform = cross(reg_fun = c(reg1, reg2), data = c(small, large))
    ),
    summ = target(
      sum_fun(data, reg),
      transform = cross(sum_fun = c(coef, residuals), reg)
    ),
    winners = target(
      min(summ),
      transform = combine(summ, .by = c(data, sum_fun))
    ),
    others = target(
      analyze(list(c(summ, data))) + 1,
      transform = combine(
        summ,
        data,
        .by = c(data, sum_fun)
      )
    ),
    final_winner = target(
      min(winners),
      transform = combine(winners)
    )
  )
  exp <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg_reg1_small = reg1(small),
    reg_reg2_small = reg2(small),
    reg_reg1_large = reg1(large),
    reg_reg2_large = reg2(large),
    summ_coef_reg_reg1_large = coef(large, reg_reg1_large),
    summ_residuals_reg_reg1_large = residuals(large, reg_reg1_large),
    summ_coef_reg_reg1_small = coef(small, reg_reg1_small),
    summ_residuals_reg_reg1_small = residuals(small, reg_reg1_small),
    summ_coef_reg_reg2_large = coef(large, reg_reg2_large),
    summ_residuals_reg_reg2_large = residuals(large, reg_reg2_large),
    summ_coef_reg_reg2_small = coef(small, reg_reg2_small),
    summ_residuals_reg_reg2_small = residuals(small, reg_reg2_small),
    winners_large_coef = min(
      summ_coef_reg_reg1_large,
      summ_coef_reg_reg2_large
    ),
    winners_small_coef = min(
      summ_coef_reg_reg1_small,
      summ_coef_reg_reg2_small
    ),
    winners_large_residuals = min(
      summ_residuals_reg_reg1_large,
      summ_residuals_reg_reg2_large
    ),
    winners_small_residuals = min(
      summ_residuals_reg_reg1_small,
      summ_residuals_reg_reg2_small
    ),
    others_large_coef = analyze(list(c(
      summ_coef_reg_reg1_large,
      summ_coef_reg_reg2_large,
      large
    ))) + 1,
    others_small_coef = analyze(list(c(
      summ_coef_reg_reg1_small,
      summ_coef_reg_reg2_small,
      small
    ))) + 1,
    others_large_residuals = analyze(list(c(
      summ_residuals_reg_reg1_large,
      summ_residuals_reg_reg2_large,
      large
    ))) + 1,
    others_small_residuals = analyze(list(c(
      summ_residuals_reg_reg1_small,
      summ_residuals_reg_reg2_small,
      small
    ))) + 1,
    final_winner = min(
      winners_large_coef,
      winners_small_coef,
      winners_large_residuals,
      winners_small_residuals
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("more map", {
  skip_on_cran()
  out <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg = target(
      reg_fun(data),
      transform = map(reg_fun = c(reg1, reg2), data = c(small, large))
    ),
    summ = target(
      sum_fun(data, reg),
      transform = map(sum_fun = c(coef, residuals), reg),
      custom1 = 123L
    ),
    winners = target(
      min(summ),
      transform = combine(summ, .by = c(sum_fun, data)),
      custom2 = 456L
    )
  )
  exp <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg_reg1_small = reg1(small),
    reg_reg2_large = reg2(large),
    summ_coef_reg_reg1_small = target(
      command = coef(small, reg_reg1_small),
      custom1 = 123L
    ),
    summ_residuals_reg_reg2_large = target(
      command = residuals(large, reg_reg2_large),
      custom1 = 123L
    ),
    winners_residuals_large = target(
      command = min(
        summ_residuals_reg_reg2_large),
      custom2 = 456L
    ),
    winners_coef_small = target(
      command = min(
        summ_coef_reg_reg1_small
      ),
      custom2 = 456L
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("map on mtcars-like workflow", {
  skip_on_cran()
  out <- drake_plan(
    data = target(
      simulate(nrows),
      transform = map(nrows = c(48, 64))
    ),
    reg = target(
      reg_fun(data),
      transform = cross(reg_fun = c(reg1, reg2), data)
    ),
    summ = target(
      sum_fun(data, reg),
      transform = cross(sum_fun = c(coef, resid), reg)
    ),
    winners = target(
      min(summ),
      transform = combine(summ, .by = c(data, sum_fun))
    )
  )
  exp <- drake_plan(
    data_48 = simulate(48),
    data_64 = simulate(64),
    reg_reg1_data_48 = reg1(data_48),
    reg_reg2_data_48 = reg2(data_48),
    reg_reg1_data_64 = reg1(data_64),
    reg_reg2_data_64 = reg2(data_64),
    summ_coef_reg_reg1_data_48 = coef(data_48, reg_reg1_data_48),
    summ_resid_reg_reg1_data_48 = resid(data_48, reg_reg1_data_48),
    summ_coef_reg_reg1_data_64 = coef(data_64, reg_reg1_data_64),
    summ_resid_reg_reg1_data_64 = resid(data_64, reg_reg1_data_64),
    summ_coef_reg_reg2_data_48 = coef(data_48, reg_reg2_data_48),
    summ_resid_reg_reg2_data_48 = resid(data_48, reg_reg2_data_48),
    summ_coef_reg_reg2_data_64 = coef(data_64, reg_reg2_data_64),
    summ_resid_reg_reg2_data_64 = resid(data_64, reg_reg2_data_64),
    winners_data_48_coef = min(
      summ_coef_reg_reg1_data_48,
      summ_coef_reg_reg2_data_48
    ),
    winners_data_64_coef = min(
      summ_coef_reg_reg1_data_64,
      summ_coef_reg_reg2_data_64
    ),
    winners_data_48_resid = min(
      summ_resid_reg_reg1_data_48,
      summ_resid_reg_reg2_data_48
    ),
    winners_data_64_resid = min(
      summ_resid_reg_reg1_data_64,
      summ_resid_reg_reg2_data_64
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("map with unequal columns", {
  skip_on_cran()
  expect_error(
    drake_plan(
      small = simulate(48),
      large = simulate(64),
      reg = target(
        reg_fun(data),
        transform = map(reg_fun = c(reg1, reg2), data = c(small, large, huge))
      )
    ),
    regexp = "uneven groupings detected in map"
  )
})

test_with_dir("map with an indicator column", {
  skip_on_cran()
  out <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg = target(
      reg_fun(data),
      transform = map(reg_fun = reg1, data = c(small, large, huge))
    ),
    trace = TRUE
  )
  exp <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg_reg1_small = target(
      command = reg1(small),
      reg_fun = "reg1",
      data = "small",
      reg = "reg_reg1_small"
    ),
    reg_reg1_large = target(
      command = reg1(large),
      reg_fun = "reg1",
      data = "large",
      reg = "reg_reg1_large"
    ),
    reg_reg1_huge = target(
      command = reg1(huge),
      reg_fun = "reg1",
      data = "huge",
      reg = "reg_reg1_huge"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("dsl and custom columns", {
  skip_on_cran()
  e <- quote(
    drake_plan(
      small = simulate(48),
      large = simulate(64),
      reg = target(
        reg_fun(data),
        transform = cross(reg_fun = c(reg1, reg2), data = c(small, large))
      ),
      summ = target(
        sum_fun(data, reg),
        transform = cross(sum_fun = c(coef, residuals), reg),
        custom1 = 123L
      ),
      winners = target(
        min(summ),
        transform = combine(summ, .by = c(data, sum_fun)),
        custom2 = 456L
      )
    )
  )
  expect_silent(plan <- eval(e))
  expect_equal(
    plan$custom1,
    c(rep(NA_integer_, 6), rep(123L, 8), rep(NA_integer_, 4))
  )
  expect_equal(
    plan$custom2,
    c(rep(NA_integer_, 14), rep(456L, 4))
  )
  illegals <- list(
    quote(target(simulate(48), transform = map(command))),
    quote(target(simulate(48), transform = map(transform))),
    quote(target(simulate(48), transform = map(target))),
    quote(target(simulate(48), transform = map(target = 123))),
    quote(target(simulate(48), transform = map(command = 123))),
    quote(target(simulate(48), transform = map(transform = 123))),
    quote(target(simulate(48), data = 123)),
    quote(target(simulate(48), reg = 123)),
    quote(target(simulate(48), reg_fun = 123)),
    quote(target(simulate(48), sum_fun = 123)),
    quote(target(simulate(48), summ = 123))
  )
  msg <- "cannot also be custom column names in the plan"
  for (illegal in illegals[1:2]) {
    e[[2]] <- illegal
    expect_error(eval(e))
  }
  for (illegal in illegals[-1:-2]) {
    e[[2]] <- illegal
    expect_error(eval(e), regexp = msg)
  }
})

test_with_dir("dsl trace", {
  skip_on_cran()
  plan <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg = target(
      reg_fun(data),
      transform = cross(reg_fun = c(reg1, reg2), data = c(small, large))
    ),
    summ = target(
      sum_fun(data, reg),
      transform = cross(sum_fun = c(coef, residuals), reg)
    ),
    winners = target(
      min(summ),
      transform = combine(data, sum_fun)
    ),
    trace = FALSE
  )
  expect_false("trace" %in% plan$target)
  expect_equal(sort(colnames(plan)), sort(c("target", "command")))
  plan <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg = target(
      reg_fun(data),
      transform = cross(reg_fun = c(reg1, reg2), data = c(small, large))
    ),
    summ = target(
      sum_fun(data, reg),
      transform = cross(sum_fun = c(coef, residuals), reg)
    ),
    winners = target(
      min(summ),
      transform = combine(data, sum_fun)
    ),
    trace = TRUE
  )
  expect_false("trace" %in% plan$target)
  expect_equal(
    sort(colnames(plan)),
    sort(c(
      "target", "command", "reg", "reg_fun", "data", "summ",
      "sum_fun", "winners"
    ))
  )
})

test_with_dir("running a dsl-generated mtcars-like plan", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  load_mtcars_example()
  rm(my_plan)
  plan <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg = target(
      reg_fun(data),
      transform = cross(data = c(small, large), reg_fun = c(reg1, reg2))
    ),
    summ = target(
      summary(reg)$sumtype,
      transform = cross(reg, sumtype = c(residuals, coefficients))
    )
  )
  expect_equal(nrow(plan), 14L)
  cache <- storr::storr_environment()
  make(plan, session_info = FALSE, cache = cache)
  config <- drake_config(plan, cache = cache)
  expect_equal(sort(justbuilt(config)), sort(plan$target))
  make(plan, session_info = FALSE, cache = cache)
  expect_equal(justbuilt(config), character(0))
})

test_with_dir("dsl .tag_out groupings", {
  skip_on_cran()
  out <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg1 = target(
      rgfun(data),
      transform = cross(data = c(small, large), .tag_out = c(reg, othergroup))
    ),
    reg2 = target(
      rgfun(data),
      transform = cross(data = c(small, large), .tag_out = reg)
    ),
    winners = target(min(reg), transform = combine(reg), a = 1),
    trace = TRUE
  )
  exp <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg1_small = target(
      command = rgfun(small),
      data = "small",
      reg1 = "reg1_small",
      reg = "reg1_small",
      othergroup = "reg1_small"
    ),
    reg1_large = target(
      command = rgfun(large),
      data = "large",
      reg1 = "reg1_large",
      reg = "reg1_large",
      othergroup = "reg1_large"
    ),
    reg2_small = target(
      command = rgfun(small),
      data = "small",
      reg = "reg2_small",
      reg2 = "reg2_small"
    ),
    reg2_large = target(
      command = rgfun(large),
      data = "large",
      reg = "reg2_large",
      reg2 = "reg2_large"
    ),
    winners = target(
      command = min(
        reg1_small,
        reg1_large,
        reg2_small,
        reg2_large
      ),
      a = 1,
      winners = "winners"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("combine() and tags", {
  skip_on_cran()
  i <- as.numeric(1:3)
  out <- drake_plan(
    x = target(1, transform = map(f = !!i, .tag_in = grp, .tag_out = targs)),
    y = target(1, transform = map(g = !!i, .tag_in = grp, .tag_out = targs)),
    z = target(
      min(targs),
      transform = combine(
        targs,
        .by = grp,
        .tag_in = im,
        .tag_out = here
      )
    ),
    trace = TRUE
  )
  exp <- drake_plan(
    x_1 = target(
      command = 1,
      f = "1",
      x = "x_1",
      grp = "x",
      targs = "x_1"
    ),
    x_2 = target(
      command = 1,
      f = "2",
      x = "x_2",
      grp = "x",
      targs = "x_2"
    ),
    x_3 = target(
      command = 1,
      f = "3",
      x = "x_3",
      grp = "x",
      targs = "x_3"
    ),
    y_1 = target(
      command = 1,
      grp = "y",
      targs = "y_1",
      g = "1",
      y = "y_1"
    ),
    y_2 = target(
      command = 1,
      grp = "y",
      targs = "y_2",
      g = "2",
      y = "y_2"
    ),
    y_3 = target(
      command = 1,
      grp = "y",
      targs = "y_3",
      g = "3",
      y = "y_3"
    ),
    z_x = target(
      command = min(x_1, x_2, x_3),
      grp = "x",
      z = "z_x",
      im = "z",
      here = "z_x"
    ),
    z_y = target(
      command = min(y_1, y_2, y_3),
      grp = "y",
      z = "z_y",
      im = "z",
      here = "z_y"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("can disable transformations in dsl", {
  skip_on_cran()
  out <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg1 = target(
      reg_fun(data),
      transform = cross(data = c(small, large), .tag_out = reg)
    ),
    reg2 = target(
      reg_fun(data),
      transform = cross(data = c(small, large), .tag_out = reg)
    ),
    winners = target(
      min(reg),
      transform = combine(data),
      a = 1
    ),
    transform = FALSE
  )
  expect_equal(
    sort(out$target),
    sort(c("small", "large", "reg1", "reg2", "winners"))
  )
})

test_with_dir("dsl with differently typed group levels", {
  skip_on_cran()
  plan1 <- drake_plan(
    analysis = target(
      analyze_data(source),
      transform = cross(source = c("source1", source2, 3))
    ),
    transform = FALSE
  )
  plan2 <- drake_plan(
    reducks = target(
      combine_analyses(analysis),
      transform = combine(analysis)
    ),
    transform = FALSE
  )
  plan <- bind_plans(plan1, plan2)
  out <- transform_plan(
    plan,
    envir = environment(),
    trace = FALSE,
    max_expand = NULL
  )
  exp <- drake_plan(
    analysis_source1 = analyze_data("source1"), # nolint
    analysis_source2 = analyze_data(source2),
    analysis_3 = analyze_data(3),
    reducks = combine_analyses(
      analysis_source1, # nolint
      analysis_source2,
      analysis_3
    )
  )
  equivalent_plans(out, exp)
  out <- transform_plan(
    plan,
    envir = environment(),
    trace = TRUE,
    max_expand = NULL
  )
  exp <- drake_plan(
    analysis_source1 = target( # nolint
      command = analyze_data("source1"),
      source = "\"source1\"",
      analysis = "analysis_source1"
    ),
    analysis_source2 = target(
      command = analyze_data(source2),
      source = "source2",
      analysis = "analysis_source2"
    ),
    analysis_3 = target(
      command = analyze_data(3),
      source = "3",
      analysis = "analysis_3"
    ),
    reducks = target(
      command = combine_analyses(
        analysis_source1, # nolint
        analysis_source2,
        analysis_3
      ),
      reducks = "reducks"
    )
  )
  expect_true(ncol(exp) > 2)
  equivalent_plans(out, exp)
})

test_with_dir("tidy eval in the DSL", {
  skip_on_cran()
  h <- function(x) {
    x
  }
  out <- drake_plan(
    x = target(
      f(char),
      trigger = trigger(condition = g(char)),
      custom = h(char),
      transform = map(char = !!letters[1:2])
    )
  )
  exp <- drake_plan(
    x_a = target(
      command = f("a"),
      trigger = trigger(
        condition = g("a")
      ),
      custom = "a"
    ),
    x_b = target(
      command = f("b"),
      trigger = trigger(
        condition = g("b")
      ),
      custom = "b"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("resource column is not a language object (#942)", {
  skip_on_cran()
  mem <- 1024
  x <- "b"
  out <- drake_plan(
    data = target(
      download_data(),
      resources = list(cores = 1, gpus = 0, mem = !!mem),
      x = "a"
    ),
    model = target(
      big_machine_learning_model(data),
      resources = list(cores = 4, gpus = 1, mem = !!mem),
      x = !!x
    )
  )
  exp <- drake_plan(
    data = target(
      command = download_data(),
      resources = list(cores = 1, gpus = 0, mem = 1024),
      x = "a"
    ),
    model = target(
      command = big_machine_learning_model(data),
      resources = list(cores = 4, gpus = 1, mem = 1024),
      x = "b"
    )
  )
  equivalent_plans(out, exp)
  out <- out$resources
  exp <- list(
    list(cores = 1, gpus = 0, mem = 1024),
    list(cores = 4, gpus = 1, mem = 1024)
  )
  expect_equal(out, exp)
})

test_with_dir("dsl: exact same plan as mtcars", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  out <- drake_plan(
    report = knitr::knit(
      drake::knitr_in("report.Rmd"),
      drake::file_out("report.md"),
      quiet = TRUE
    ),
    small = simulate(48),
    large = simulate(64),
    regression1 = target(
      reg1(data),
      transform = map(data = c(small, large), .tag_out = reg)
    ),
    regression2 = target(
      reg2(data),
      transform = map(data = c(small, large), .tag_out = reg)
    ),
    summ = target(
      suppressWarnings(summary(reg$residuals)),
      transform = map(reg)
    ),
    coef = target(
      suppressWarnings(summary(reg))$coefficients,
      transform = map(reg)
    )
  )
  load_mtcars_example()
  equivalent_plans(out, my_plan)
})

test_with_dir("dsl: no NA levels in combine()", {
  skip_on_cran()
  out <- drake_plan(
    data_sim = target(
      sim_data(mean = x, sd = y),
      transform = cross(x = c(1, 2), y = c(3, 4), .tag_out = c(data, local))
    ),
    data_download = target(
      download_data(url = x),
      transform = map(
        x = c("http://url_1", "http://url_2"),
        .tag_out = c(real, data)
      )
    ),
    data_pkg = target(
      load_data_from_package(pkg = x),
      transform = map(
        x = c("gapminder", "Ecdat"),
        .tag_out = c(local, real, data)
      )
    ),
    summaries = target(
      compare_ds(data_sim),
      transform = combine(data_sim, .by = local)
    )
  )
  exp <- drake_plan(
    data_sim_1_3 = sim_data(mean = 1, sd = 3),
    data_sim_2_3 = sim_data(mean = 2, sd = 3),
    data_sim_1_4 = sim_data(mean = 1, sd = 4),
    data_sim_2_4 = sim_data(mean = 2, sd = 4),
    data_download_http...url_1 = download_data(url = "http://url_1"),
    data_download_http...url_2 = download_data(url = "http://url_2"),
    data_pkg_gapminder = load_data_from_package(pkg = "gapminder"),
    data_pkg_Ecdat = load_data_from_package(pkg = "Ecdat"),
    summaries_data_sim_1_3 = compare_ds(data_sim_1_3),
    summaries_data_sim_1_4 = compare_ds(data_sim_1_4),
    summaries_data_sim_2_3 = compare_ds(data_sim_2_3),
    summaries_data_sim_2_4 = compare_ds(data_sim_2_4)
  )
  equivalent_plans(out, exp)
})

test_with_dir("trace has correct provenance", {
  out <- drake_plan(
    trace = TRUE,
    a = target(x, transform = map(x = c(1, 1), y = c(3, 3))),
    b = target(a, transform = map(a)),
    c = target(b, transform = map(b)),
    d0 = target(b, transform = cross(b, c)),
    e = target(c, transform = map(c)),
    f = target(c, transform = map(c)),
    g = target(b, transform = map(b)),
    h = target(a, transform = map(a)),
    i = target(e, transform = combine(e)),
    j = target(f, transform = combine(f))
  )
  exp <- drake_plan(
    a_1_3 = target(
      command = 1,
      x = "1",
      y = "3",
      a = "a_1_3"
    ),
    a_1_3_2 = target(
      command = 1,
      x = "1",
      y = "3",
      a = "a_1_3_2"
    ),
    b_a_1_3 = target(
      command = a_1_3,
      x = "1",
      y = "3",
      a = "a_1_3",
      b = "b_a_1_3"
    ),
    b_a_1_3_2 = target(
      command = a_1_3_2,
      x = "1",
      y = "3",
      a = "a_1_3_2",
      b = "b_a_1_3_2"
    ),
    c_b_a_1_3 = target(
      command = b_a_1_3,
      x = "1",
      y = "3",
      a = "a_1_3",
      b = "b_a_1_3",
      c = "c_b_a_1_3"
    ),
    c_b_a_1_3_2 = target(
      command = b_a_1_3_2,
      x = "1",
      y = "3",
      a = "a_1_3_2",
      b = "b_a_1_3_2",
      c = "c_b_a_1_3_2"
    ),
    d0_b_a_1_3_c_b_a_1_3 = target(
      command = b_a_1_3,
      x = "1",
      y = "3",
      a = "a_1_3",
      b = "b_a_1_3",
      c = "c_b_a_1_3",
      d0 = "d0_b_a_1_3_c_b_a_1_3"
    ),
    d0_b_a_1_3_c_b_a_1_3_2 = target(
      command = b_a_1_3,
      x = "1",
      y = "3",
      a = "a_1_3",
      b = "b_a_1_3",
      c = "c_b_a_1_3_2",
      d0 = "d0_b_a_1_3_c_b_a_1_3_2"
    ),
    d0_b_a_1_3_2_c_b_a_1_3 = target(
      command = b_a_1_3_2,
      x = "1",
      y = "3",
      a = "a_1_3_2",
      b = "b_a_1_3_2",
      c = "c_b_a_1_3",
      d0 = "d0_b_a_1_3_2_c_b_a_1_3"
    ),
    d0_b_a_1_3_2_c_b_a_1_3_2 = target(
      command = b_a_1_3_2,
      x = "1",
      y = "3",
      a = "a_1_3_2",
      b = "b_a_1_3_2",
      c = "c_b_a_1_3_2",
      d0 = "d0_b_a_1_3_2_c_b_a_1_3_2"
    ),
    e_c_b_a_1_3 = target(
      command = c_b_a_1_3,
      x = "1",
      y = "3",
      a = "a_1_3",
      b = "b_a_1_3",
      c = "c_b_a_1_3",
      e = "e_c_b_a_1_3"
    ),
    e_c_b_a_1_3_2 = target(
      command = c_b_a_1_3_2,
      x = "1",
      y = "3",
      a = "a_1_3_2",
      b = "b_a_1_3_2",
      c = "c_b_a_1_3_2",
      e = "e_c_b_a_1_3_2"
    ),
    f_c_b_a_1_3 = target(
      command = c_b_a_1_3,
      x = "1",
      y = "3",
      a = "a_1_3",
      b = "b_a_1_3",
      c = "c_b_a_1_3",
      f = "f_c_b_a_1_3"
    ),
    f_c_b_a_1_3_2 = target(
      command = c_b_a_1_3_2,
      x = "1",
      y = "3",
      a = "a_1_3_2",
      b = "b_a_1_3_2",
      c = "c_b_a_1_3_2",
      f = "f_c_b_a_1_3_2"
    ),
    g_b_a_1_3 = target(
      command = b_a_1_3,
      x = "1",
      y = "3",
      a = "a_1_3",
      b = "b_a_1_3",
      g = "g_b_a_1_3"
    ),
    g_b_a_1_3_2 = target(
      command = b_a_1_3_2,
      x = "1",
      y = "3",
      a = "a_1_3_2",
      b = "b_a_1_3_2",
      g = "g_b_a_1_3_2"
    ),
    h_a_1_3 = target(
      command = a_1_3,
      x = "1",
      y = "3",
      a = "a_1_3",
      h = "h_a_1_3"
    ),
    h_a_1_3_2 = target(
      command = a_1_3_2,
      x = "1",
      y = "3",
      a = "a_1_3_2",
      h = "h_a_1_3_2"
    ),
    i = target(
      command = list(e_c_b_a_1_3, e_c_b_a_1_3_2),
      x = "1",
      y = "3",
      i = "i"
    ),
    j = target(
      command = list(f_c_b_a_1_3, f_c_b_a_1_3_2),
      x = "1",
      y = "3",
      j = "j"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("row order does not matter", {
  plan1 <- drake_plan(
    coef = target(
      suppressWarnings(summary(reg))$coefficients,
      transform = map(reg)
    ),
    summ = target(
      suppressWarnings(summary(reg$residuals)),
      transform = map(reg)
    ),
    report = knit(knitr_in("report.Rmd"), file_out("report.md"), quiet = TRUE),
    regression1 = target(
      reg1(data),
      transform = map(data = c(small, large), .tag_out = reg)
    ),
    regression2 = target(
      reg2(data),
      transform = map(data = c(small, large), .tag_out = reg)
    ),
    small = simulate(48),
    large = simulate(64),
    trace = TRUE
  )
  plan2 <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    report = knit(knitr_in("report.Rmd"), file_out("report.md"), quiet = TRUE),
    regression2 = target(
      reg2(data),
      transform = map(data = c(small, large), .tag_out = reg)
    ),
    regression1 = target(
      reg1(data),
      transform = map(data = c(small, large), .tag_out = reg)
    ),
    summ = target(
      suppressWarnings(summary(reg$residuals)),
      transform = map(reg)
    ),
    coef = target(
      suppressWarnings(summary(reg))$coefficients,
      transform = map(reg)
    ),
    trace = TRUE
  )
  expect_equal(nrow(plan1), 15L)
  equivalent_plans(plan1, plan2)
})

test_with_dir("same test (row order) different plan", {
  skip_on_cran()
  plan1 <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg = target(
      reg_fun(data),
      transform = cross(reg_fun = c(reg1, reg2), data = c(small, large))
    ),
    summ = target(
      sum_fun(data, reg),
      transform = cross(sum_fun = c(coef, residuals), reg)
    ),
    winners = target(
      min(summ),
      transform = combine(summ, .by = c(data, sum_fun))
    ),
    others = target(
      analyze(list(c(summ), c(data))),
      transform = combine(
        summ,
        data,
        .by = c(data, sum_fun)
      )
    ),
    final_winner = target(
      min(winners),
      transform = combine(winners)
    )
  )
  plan2 <- drake_plan(
    final_winner = target(
      min(winners),
      transform = combine(winners)
    ),
    reg = target(
      reg_fun(data),
      transform = cross(reg_fun = c(reg1, reg2), data = c(small, large))
    ),
    small = simulate(48),
    summ = target(
      sum_fun(data, reg),
      transform = cross(sum_fun = c(coef, residuals), reg)
    ),
    others = target(
      analyze(list(c(summ), c(data))),
      transform = combine(
        summ,
        data,
        .by = c(data, sum_fun)
      )
    ),
    winners = target(
      min(summ),
      transform = combine(summ, .by = c(data, sum_fun))
    ),
    large = simulate(64)
  )
  expect_equal(nrow(plan1), 23L)
  equivalent_plans(plan1, plan2)
})

test_with_dir("gh #696", {
  skip_on_cran()
  my_split <- function(from, stem, n) {
    suffixes <- with(
      expand.grid(y = letters, x = letters),
      paste0(x, y)
    )[1:n]
    out.files <- paste0(stem, suffixes)
    out <- rlang::quo({
      file_in(!!from)
      file_out(!!out.files)
      system2(
        "split",
        c(paste0("-n r/", !!n),
          !!from,
          !!stem)
      )
    })
    out <- quo_squash(out)
  }
  manysplits <- paste0("lf", 1:2, ".txt")
  out <- drake_plan(
    splits = target(!!my_split(f, f, 3), transform = map(f = !!manysplits))
  )
  exp <- drake_plan(
    splits_lf1.txt = {
      file_in("lf1.txt")
      file_out(c("lf1.txtaa", "lf1.txtab", "lf1.txtac"))
      system2("split", c(paste0("-n r/", 3), "lf1.txt", "lf1.txt"))
    },
    splits_lf2.txt = {
      file_in("lf2.txt")
      file_out(c("lf2.txtaa", "lf2.txtab", "lf2.txtac"))
      system2("split", c(paste0("-n r/", 3), "lf2.txt", "lf2.txt"))
    }
  )
  equivalent_plans(out, exp)
})

test_with_dir("transformations in triggers", {
  skip_on_cran()
  out <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg = target(
      reg_fun(data),
      trigger = trigger(change = reg_fun(data)),
      transform = cross(reg_fun = c(reg1, reg2), data = c(small, large))
    ),
    summ = target(
      sum_fun(data, reg),
      trigger = trigger(change = sum_fun(data, reg)),
      transform = cross(sum_fun = c(coef, residuals), reg)
    ),
    winners = target(
      min(summ),
      trigger = trigger(change = min(summ)),
      transform = combine(summ, .by = c(data, sum_fun))
    ),
    others = target(
      analyze(list(c(summ), c(data))),
      trigger = trigger(change = analyze(list(c(summ), c(data)))),
      transform = combine(
        summ,
        data,
        .by = c(data, sum_fun)
      )
    ),
    final_winner = target(
      min(winners),
      trigger = trigger(change = min(winners)),
      transform = combine(winners)
    )
  )
  exp <- drake_plan(
    small = target(
      command = simulate(48),
      trigger = NA
    ),
    large = target(
      command = simulate(64),
      trigger = NA
    ),
    reg_reg1_small = target(
      command = reg1(small),
      trigger = trigger(
        change = reg1(small)
      )
    ),
    reg_reg2_small = target(
      command = reg2(small),
      trigger = trigger(
        change = reg2(small)
      )
    ),
    reg_reg1_large = target(
      command = reg1(large),
      trigger = trigger(
        change = reg1(large)
      )
    ),
    reg_reg2_large = target(
      command = reg2(large),
      trigger = trigger(
        change = reg2(large)
      )
    ),
    summ_coef_reg_reg1_large = target(
      command = coef(large, reg_reg1_large),
      trigger = trigger(
        change = coef(large, reg_reg1_large)
      )
    ),
    summ_residuals_reg_reg1_large = target(
      command = residuals(large, reg_reg1_large),
      trigger = trigger(
        change = residuals(large, reg_reg1_large)
      )
    ),
    summ_coef_reg_reg1_small = target(
      command = coef(small, reg_reg1_small),
      trigger = trigger(
        change = coef(small, reg_reg1_small)
      )
    ),
    summ_residuals_reg_reg1_small = target(
      command = residuals(small, reg_reg1_small),
      trigger = trigger(
        change = residuals(small, reg_reg1_small)
      )
    ),
    summ_coef_reg_reg2_large = target(
      command = coef(large, reg_reg2_large),
      trigger = trigger(
        change = coef(large, reg_reg2_large)
      )
    ),
    summ_residuals_reg_reg2_large = target(
      command = residuals(large, reg_reg2_large),
      trigger = trigger(
        change = residuals(large, reg_reg2_large)
      )
    ),
    summ_coef_reg_reg2_small = target(
      command = coef(small, reg_reg2_small),
      trigger = trigger(
        change = coef(small, reg_reg2_small)
      )
    ),
    summ_residuals_reg_reg2_small = target(
      command = residuals(small, reg_reg2_small),
      trigger = trigger(
        change = residuals(small, reg_reg2_small)
      )
    ),
    winners_large_coef = target(
      command = min(
        summ_coef_reg_reg1_large,
        summ_coef_reg_reg2_large
      ),
      trigger = trigger(
        change = min(
          summ_coef_reg_reg1_large,
          summ_coef_reg_reg2_large
        )
      )
    ),
    winners_small_coef = target(
      command = min(
        summ_coef_reg_reg1_small,
        summ_coef_reg_reg2_small
      ),
      trigger = trigger(
        change = min(
          summ_coef_reg_reg1_small,
          summ_coef_reg_reg2_small
        )
      )
    ),
    winners_large_residuals = target(
      command = min(
        summ_residuals_reg_reg1_large,
        summ_residuals_reg_reg2_large
      ),
      trigger = trigger(
        change = min(
          summ_residuals_reg_reg1_large,
          summ_residuals_reg_reg2_large
        )
      )
    ),
    winners_small_residuals = target(
      command = min(
        summ_residuals_reg_reg1_small,
        summ_residuals_reg_reg2_small
      ),
      trigger = trigger(
        change = min(
          summ_residuals_reg_reg1_small,
          summ_residuals_reg_reg2_small
        )
      )
    ),
    others_large_coef = target(
      command = analyze(list(
        c(summ_coef_reg_reg1_large, summ_coef_reg_reg2_large),
        c(large)
      )),
      trigger = trigger(
        change = analyze(list(
          c(summ_coef_reg_reg1_large, summ_coef_reg_reg2_large),
          c(large)
        ))
      )
    ),
    others_small_coef = target(
      command = analyze(list(
        c(summ_coef_reg_reg1_small, summ_coef_reg_reg2_small),
        c(small)
      )),
      trigger = trigger(
        change = analyze(list(
          c(summ_coef_reg_reg1_small, summ_coef_reg_reg2_small),
          c(small)
        ))
      )
    ),
    others_large_residuals = target(
      command = analyze(list(
        c(summ_residuals_reg_reg1_large, summ_residuals_reg_reg2_large),
        c(large)
      )),
      trigger = trigger(
        change = analyze(list(
          c(summ_residuals_reg_reg1_large, summ_residuals_reg_reg2_large),
          c(large)
        ))
      )
    ),
    others_small_residuals = target(
      command = analyze(list(
        c(summ_residuals_reg_reg1_small, summ_residuals_reg_reg2_small),
        c(small)
      )),
      trigger = trigger(
        change = analyze(list(
          c(summ_residuals_reg_reg1_small, summ_residuals_reg_reg2_small),
          c(small)
        ))
      )
    ),
    final_winner = target(
      command = min(
        winners_large_coef, winners_small_coef, winners_large_residuals,
        winners_small_residuals
      ),
      trigger = trigger(
        change = min(
          winners_large_coef, winners_small_coef, winners_large_residuals,
          winners_small_residuals
        )
      )
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir(".id = FALSE", {
  skip_on_cran()
  x_ <- letters[1:2]
  y_ <- letters[3:4]
  z_ <- letters[11:14]
  out <- drake_plan(
    a = target(c(x, y), transform = cross(x = !!x_, y = !!y_, .id = FALSE)),
    b = target(c(a, z), transform = map(a, z = !!z_, .id = FALSE)),
    d = target(b, transform = combine(b, .by = x, .id = FALSE))
  )
  exp <- drake_plan(
    a = c("a", "c"),
    a_2 = c("b", "c"),
    a_3 = c("a", "d"),
    a_4 = c("b", "d"),
    b = c(a, "k"),
    b_2 = c(a_2, "l"),
    b_3 = c(a_3, "m"),
    b_4 = c(a_4, "n"),
    d = list(b, b_3),
    d_2 = list(b_2, b_4)
  )
  equivalent_plans(out, exp)
})

test_with_dir("(1) .id = syms. (2) map() finds the correct cross() syms", {
  skip_on_cran()
  x_ <- letters[1:2]
  y_ <- letters[3:4]
  z_ <- letters[11:12]
  out <- drake_plan(
    A = target(
      c(x, y, z),
      transform = cross(x = !!x_, y = !!y_, z = !!z_, .id = z)
    ),
    B = target(c(A, y, z), transform = map(A, y, z, .id = c(y, z))),
    C = target(B, transform = combine(B, .by = c(x, y), .id = bad))
  )
  # nolint start
  exp <- drake_plan(
    A_k = c("a", "c", "k"),
    A_k_2 = c("b", "c", "k"),
    A_k_3 = c("a", "d", "k"),
    A_k_4 = c("b", "d", "k"),
    A_l = c("a", "c", "l"),
    A_l_2 = c("b", "c", "l"),
    A_l_3 = c("a", "d", "l"),
    A_l_4 = c("b", "d", "l"),
    B_c_k = c(A_k, "c", "k"),
    B_c_k_2 = c(A_k_2, "c", "k"),
    B_d_k = c(A_k_3, "d", "k"),
    B_d_k_2 = c(A_k_4, "d", "k"),
    B_c_l = c(A_l, "c", "l"),
    B_c_l_2 = c(A_l_2, "c", "l"),
    B_d_l = c(A_l_3, "d", "l"),
    B_d_l_2 = c(A_l_4, "d", "l"),
    C = list(B_c_k, B_c_l),
    C_2 = list(B_c_k_2, B_c_l_2),
    C_3 = list(B_d_k, B_d_l),
    C_4 = list(B_d_k_2, B_d_l_2)
  )
  # nolint end
  equivalent_plans(out, exp)
})

test_with_dir("upstream .id columns are available", {
  skip_on_cran()
  factor_a_ <- as.character(c(4, 5, 6, 7, 8))
  factor_b_ <- "2"
  out <- drake_plan(
    raw_data = get_data(),
    data = clean_data(raw_data),
    analysis = target(
      data %>%
        filter(factor_a == factor_a_ & factor_b == factor_b_),
      transform = cross(factor_a_ = !!factor_a_, factor_b_ = !!factor_b_)
    ),
    summary = target(
      my_summarize(analysis),
      transform = map(analysis, .id = c(factor_a_, factor_b_))
    ),
    results = target(bind_rows(summary), transform = combine(summary))
  )
  # nolint start
  exp <- drake_plan(
    raw_data = get_data(),
    data = clean_data(raw_data),
    analysis_4_2 = data %>% filter(factor_a == "4" & factor_b == "2"),
    analysis_5_2 = data %>% filter(factor_a == "5" & factor_b == "2"),
    analysis_6_2 = data %>% filter(factor_a == "6" & factor_b == "2"),
    analysis_7_2 = data %>% filter(factor_a == "7" & factor_b == "2"),
    analysis_8_2 = data %>% filter(factor_a == "8" & factor_b == "2"),
    summary_4_2 = my_summarize(analysis_4_2),
    summary_5_2 = my_summarize(analysis_5_2),
    summary_6_2 = my_summarize(analysis_6_2),
    summary_7_2 = my_summarize(analysis_7_2),
    summary_8_2 = my_summarize(analysis_8_2),
    results = bind_rows(
      summary_4_2, summary_5_2, summary_6_2,
      summary_7_2, summary_8_2
    )
  )
  # nolint end
  equivalent_plans(out, exp)
})

test_with_dir("repeated maps do not duplicate targets", {
  skip_on_cran()
  x_ <- rep("a", 2)
  y_ <- rep("b", 2)
  out <- drake_plan(
    A = target(x, transform = map(x = !!x_, .id = FALSE)),
    B = target(c(A, x), transform = map(A, x, .id = FALSE)),
    C = target(y, transform = map(y = !!y_, .id = FALSE)),
    D = target(c(A, B, C, x, y), transform = map(A, B, C, x, y, .id = FALSE))
  )
  exp <- drake_plan(
    A = "a",
    A_2 = "a",
    B = c(A, "a"),
    B_2 = c(A_2, "a"),
    C = "b",
    C_2 = "b",
    D = c(A, B, C, "a", "b"),
    D_2 = c(A_2, B_2, C_2, "a", "b")
  )
  equivalent_plans(out, exp)
})

test_with_dir("unequal trace vars are not duplicated in map()", {
  skip_on_cran()
  inputs <- lapply(LETTERS[1:4], as.symbol)
  types <- rep(c(1, 2), each = 2)
  out <- drake_plan(
    wide1 = target(
      ez_parallel(a),
      transform = map(a = !!inputs, type = !!types)),
    prelim = target(
      preliminary(wide1),
      transform = combine(wide1, .by = type)),
    main = target(
      expensive_calc(prelim),
      transform = map(prelim)
    ),
    format = target(
      postformat(prelim, main),
      transform = map(prelim, main)
    )
  )
  exp <- drake_plan(
    wide1_A_1 = ez_parallel(A),
    wide1_B_1 = ez_parallel(B),
    wide1_C_2 = ez_parallel(C),
    wide1_D_2 = ez_parallel(D),
    prelim_1 = preliminary(wide1_A_1, wide1_B_1),
    prelim_2 = preliminary(wide1_C_2, wide1_D_2),
    main_prelim_1 = expensive_calc(prelim_1),
    main_prelim_2 = expensive_calc(prelim_2),
    format_prelim_1_main_prelim_1 = postformat(prelim_1, main_prelim_1),
    format_prelim_2_main_prelim_2 = postformat(prelim_2, main_prelim_2)
  )
  equivalent_plans(out, exp)
})

test_with_dir("commands from combine() produce the correct values", {
  skip_on_cran()
  x_ <- letters[1:2]
  plan <- drake_plan(
    A = target(x, transform = map(x = !!x_)),
    B = target(A, transform = combine(A)),
    C = target(list(A), transform = combine(A)),
    trace = TRUE
  )
  cache <- storr::storr_environment()
  make(plan, cache = cache, session_info = FALSE)
  exp <- list("a", "b")
  expect_equal(unname(readd(B, cache = cache)), exp)
  expect_equal(unname(readd(C, cache = cache)), exp)
})

test_with_dir("grids", {
  grid <- data.frame(
    z = c(5, 6),
    w = c("7", "8"),
    v = c("a", "b"),
    stringsAsFactors = FALSE
  )
  grid$v <- rlang::syms(grid$v)
  out <- drake_plan(
    a = target(
      1 + f(x, y, z, w, v),
      transform = map(x = c(1, 2), y = c(3, 4), .data = !!grid)
    )
  )
  exp <- drake_plan(
    a_1_3_5_7_a = 1 + f(1, 3, 5, "7", a),
    a_2_4_6_8_b = 1 + f(2, 4, 6, "8", b)
  )
  equivalent_plans(out, exp)
})

test_with_dir("empty grids", {
  skip_on_cran()
  grid <- data.frame(
    z = c(5, 6),
    w = c("7", "8"),
    v = c("a", "b"),
    stringsAsFactors = FALSE
  )
  grid$v <- rlang::syms(grid$v)
  expect_error(
    out <- drake_plan(
      a = target(
        1 + f(x, y, z, w, v),
        transform = map(
          x = c(),
          y = c(),
          .data = !!grid[logical(0), , drop = FALSE] # nolint
        )
      )
    ),
    regexp = "grouping variable"
  )
})

test_with_dir("grid for GitHub issue 697", {
  skip_on_cran()
  grid <- expand.grid(
    group = c("G1", "G2"),
    rep = c("R1", "R2", "R3", "R4", "R5", "R6"),
    stringsAsFactors = FALSE
  )
  grid <- grid[!(grid$group == "G2" & grid$rep %in% c("R5", "R6")), ]
  out <- drake_plan(
    s_load = target(load_csv(group, rep), transform = map(.data = !!grid))
  )
  exp <- drake_plan(
    s_load_G1_R1 = load_csv("G1", "R1"),
    s_load_G2_R1 = load_csv("G2", "R1"),
    s_load_G1_R2 = load_csv("G1", "R2"),
    s_load_G2_R2 = load_csv("G2", "R2"),
    s_load_G1_R3 = load_csv("G1", "R3"),
    s_load_G2_R3 = load_csv("G2", "R3"),
    s_load_G1_R4 = load_csv("G1", "R4"),
    s_load_G2_R4 = load_csv("G2", "R4"),
    s_load_G1_R5 = load_csv("G1", "R5"),
    s_load_G1_R6 = load_csv("G1", "R6")
  )
  equivalent_plans(out, exp)
})

test_with_dir("grid for GitHub issue 710", {
  skip_on_cran()
  inputs <- lapply(LETTERS[1:5], as.symbol)
  types <- rep(c(1, 2), length.out = 5)
  df <- data.frame(
    serial_ = paste0("serial_", types),
    wide_ = paste0("wide_", inputs),
    stringsAsFactors = FALSE
  )
  for (col in colnames(df)) {
    df[[col]] <- rlang::syms(df[[col]])
  }
  out <- drake_plan(
    wide = target(
      ez_parallel(a),
      transform = map(a = !!inputs, type = !!types)
    ),
    serial = target(
      expensive_calc(wide),
      transform = combine(wide, .by = type)
    ),
    dist = target(
      distribute_results(serial_, wide_),
      transform = map(.data = !!df)
    )
  )
  exp <- drake_plan(
    wide_A_1 = ez_parallel(A),
    wide_B_2 = ez_parallel(B),
    wide_C_1 = ez_parallel(C),
    wide_D_2 = ez_parallel(D),
    wide_E_1 = ez_parallel(E),
    serial_1 = expensive_calc(wide_A_1, wide_C_1, wide_E_1),
    serial_2 = expensive_calc(wide_B_2, wide_D_2),
    dist_serial_1_wide_A = distribute_results(serial_1, wide_A),
    dist_serial_2_wide_B = distribute_results(serial_2, wide_B),
    dist_serial_1_wide_C = distribute_results(serial_1, wide_C),
    dist_serial_2_wide_D = distribute_results(serial_2, wide_D),
    dist_serial_1_wide_E = distribute_results(serial_1, wide_E)
  )
  equivalent_plans(out, exp)
})

test_with_dir("combine() with symbols instead of calls", {
  skip_on_cran()
  out <- drake_plan(
    data = target(
      get_data(param),
      transform = map(param = c(1, 2))
    ),
    results = target(
      .data %>%
        select(data),
      transform = combine(data)
    )
  )
  exp <- drake_plan(
    data_1 = get_data(1),
    data_2 = get_data(2),
    results = .data %>% select(data_1, data_2)
  )
  equivalent_plans(out, exp)
})

test_with_dir("combine() with complicated calls", {
  skip_on_cran()
  out <- drake_plan(
    data = target(
      get_data(param),
      transform = map(param = c(1, 2))
    ),
    results = target(
      .data %>%
        c(min(0, data, na.rm = FALSE), 2),
      transform = combine(data)
    )
  )
  exp <- drake_plan(
    data_1 = get_data(1),
    data_2 = get_data(2),
    results = .data %>% c(min(0, data_1, data_2, na.rm = FALSE), 2)
  )
  equivalent_plans(out, exp)
})

test_with_dir("invalid splitting var", {
  skip_on_cran()
  expect_error(
    out <- drake_plan(
      data = target(x, transform = map(x = c(1, 2)), nothing = NA),
      results = target(
        data,
        transform = combine(data, .by = nothing)
      )
    ),
    regexp = "grouping variable"
  )
})

test_with_dir("uneven combinations", {
  skip_on_cran()
  out <- drake_plan(
    data1 = target(
      sim_data1(mean = x, sd = y, skew = z),
      transform = map(x = c(1, 2), y = c(3, 4))
    ),
    data2 = target(
      sim_data2(mean = x, sd = y, skew = z),
      transform = cross(x = c(1, 2), y = c(3, 4))
    ),
    combined = target(
      bind_rows(data1, data2, .id = "id") %>%
        arrange(sd) %>%
        head(n = 400),
      transform = combine(data1, data2, .by = c(x, y))
    )
  )
  exp <- drake_plan(
    data1_1_3 = sim_data1(mean = 1, sd = 3, skew = z),
    data1_2_4 = sim_data1(mean = 2, sd = 4, skew = z),
    data2_1_3 = sim_data2(mean = 1, sd = 3, skew = z),
    data2_2_3 = sim_data2(mean = 2, sd = 3, skew = z),
    data2_1_4 = sim_data2(mean = 1, sd = 4, skew = z),
    data2_2_4 = sim_data2(mean = 2, sd = 4, skew = z),
    combined_1_3 = bind_rows(data1_1_3, data2_1_3, .id = "id") %>%
      arrange(sd) %>%
      head(n = 400),
    combined_2_4 = bind_rows(data1_2_4, data2_2_4, .id = "id") %>%
      arrange(sd) %>%
      head(n = 400)
  )
  equivalent_plans(out, exp)
})

test_with_dir("dates in the DSL", {
  skip_on_cran()
  dates <- seq(as.Date("2019-01-01"), as.Date("2019-01-03"), by = 1)
  plan <- drake_plan(
    y = target(d, transform = map(d = !!dates, .id = FALSE))
  )
  cache <- storr::storr_environment()
  make(plan, cache = cache, session_info = FALSE)
  expect_true(inherits(cache$get("y"), "Date"))
})

test_with_dir(".id_chr with map()", {
  skip_on_cran()
  out <- drake_plan(
    data = target(
      get_data(param),
      transform = map(param = c(123, 456))
    ),
    model = target(
      save_model_hdf5(fit_model(data), file_out(!!sprintf("%s.h5", .id_chr))),
      transform = map(data, .id = param)
    ),
    result = target(
      predict(load_model_hdf5(file_in(
        !!sprintf("%s.h5", deparse(substitute(model)))
      ))),
      transform = map(model, .id = param)
    )
  )
  exp <- drake_plan(
    data_123 = get_data(123),
    data_456 = get_data(456),
    model_123 = save_model_hdf5(
      fit_model(data_123),
      file_out("model_123.h5")
    ),
    model_456 = save_model_hdf5(
      fit_model(data_456),
      file_out("model_456.h5")
    ),
    result_123 = predict(load_model_hdf5(file_in("model_123.h5"))),
    result_456 = predict(load_model_hdf5(file_in("model_456.h5")))
  )
  equivalent_plans(out, exp)
})

test_with_dir(".id_chr with combine()", {
  skip_on_cran()
  if (!grepl("!!", deparse(quote(!!x)), fixed = TRUE)) {
    skip(
      paste(
        "R version",
        paste0(R.version$major, ".", R.version$minor),
        "deparses !!x to",
        deparse(quote(!!x))
      )
    )
  }
  out <- drake_plan(
    y = target(
      f(x),
      transform = cross(f = c(a, b), x = c(1, 2))
    ),
    report = target(
      rmarkdown::render(
        knitr_in(!!paste0(.id_chr, ".Rmd")),
        file_out(!!paste0(.id_chr, ".html"))
      ),
      transform = combine(y, .by = f)
    )
  )
  exp <- drake_plan(
    y_a_1 = a(1),
    y_b_1 = b(1),
    y_a_2 = a(2),
    y_b_2 = b(2),
    report_a = rmarkdown::render(
      knitr_in("report_a.Rmd"),
      file_out("report_a.html")
    ),
    report_b = rmarkdown::render(
      knitr_in("report_b.Rmd"),
      file_out("report_b.html")
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("max_expand", {
  skip_on_cran()
  out <- drake_plan(
    data = target(
      get_data(source),
      transform = map(source = !!seq_len(100))
    ),
    analysis = target(
      fn(data, param),
      transform = cross(data, fn = !!letters, param = !!seq_len(100))
    ),
    result = target(
      bind_rows(analysis),
      transform = combine(analysis, .by = fn)
    ),
    max_expand = 3
  )
  exp <- drake_plan(
    data_1L = get_data(1L),
    data_50L = get_data(50L),
    data_100L = get_data(100L),
    analysis_a_1L_data_1L = a(data_1L, 1L),
    analysis_z_50L_data_50L = z(data_50L, 50L),
    analysis_z_100L_data_100L = z(data_100L, 100L),
    result_a = bind_rows(analysis_a_1L_data_1L),
    result_z = bind_rows(analysis_z_50L_data_50L, analysis_z_100L_data_100L)
  )
  equivalent_plans(out, exp)
})

test_with_dir("max_expand with a .data grid for map()", {
  skip_on_cran()
  args <- data.frame(x = 1:26, y = letters, stringsAsFactors = TRUE)
  out <- drake_plan(
    data = target(
      get_data(x, y),
      transform = map(.data = !!args)
    ),
    max_expand = 5
  )
  exp <- drake_plan(
    data_1L_a = get_data(1L, "a"),
    data_7L_g = get_data(7L, "g"),
    data_13L_m = get_data(13L, "m"),
    data_19L_s = get_data(19L, "s"),
    data_26L_z = get_data(26L, "z")
  )
  equivalent_plans(out, exp)
})

test_with_dir("max_expand with a .data grid for cross()", {
  skip_on_cran()
  args <- data.frame(x = seq_len(26), y = letters, stringsAsFactors = TRUE)
  out <- drake_plan(
    data = target(
      get_data(x, y, z),
      transform = cross(
        .data = !!args,
        z = c(5L, 6L, 7L, 8L),
        .id = c(x, y, z)
      )
    ),
    max_expand = 8
  )
  exp <- drake_plan(
    data_1L_a_5L = get_data(1L, "a", 5L),
    data_19L_d_7L = get_data(19L, "d", 7L),
    data_12L_h_5L = get_data(12L, "h", 5L),
    data_4L_l_7L = get_data(4L, "l", 7L),
    data_23L_o_5L = get_data(23L, "o", 5L),
    data_15L_s_7L = get_data(15L, "s", 7L),
    data_8L_w_5L = get_data(8L, "w", 5L),
    data_26L_z_8L = get_data(26L, "z", 8L)
  )
  equivalent_plans(out, exp)
})

test_with_dir("basic splitting", {
  skip_on_cran()
  out <- drake_plan(
    large_data = get_data(),
    slice_analysis = target(
      large_data %>%
        analyze(),
      transform = split(large_data, slices = 4)
    ),
    results = target(
      rbind(slice_analysis),
      transform = combine(slice_analysis)
    )
  )
  exp <- drake_plan(
    large_data = get_data(),
    slice_analysis_1 = drake_slice(
      data = large_data, slices = 4, index = 1) %>% analyze(),
    slice_analysis_2 = drake_slice(
      data = large_data, slices = 4, index = 2) %>% analyze(),
    slice_analysis_3 = drake_slice(
      data = large_data, slices = 4, index = 3) %>% analyze(),
    slice_analysis_4 = drake_slice(
      data = large_data, slices = 4, index = 4) %>% analyze(),
    results = rbind(
      slice_analysis_1, slice_analysis_2, slice_analysis_3,
      slice_analysis_4
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("splitting with all args", {
  skip_on_cran()
  out <- drake_plan(
    large_data = get_data(),
    slice_analysis = target(
      large_data %>%
        analyze(),
      transform = split(large_data, margin = 2, drop = TRUE, slices = 4)
    ),
    results = target(
      rbind(slice_analysis),
      transform = combine(slice_analysis)
    )
  )
  exp <- drake_plan(
    large_data = get_data(),
    slice_analysis_1 = drake_slice(
      data = large_data, slices = 4, index = 1, margin = 2,
      drop = TRUE
    ) %>% analyze(),
    slice_analysis_2 = drake_slice(
      data = large_data, slices = 4, index = 2, margin = 2,
      drop = TRUE
    ) %>% analyze(),
    slice_analysis_3 = drake_slice(
      data = large_data, slices = 4, index = 3, margin = 2,
      drop = TRUE
    ) %>% analyze(),
    slice_analysis_4 = drake_slice(
      data = large_data, slices = 4, index = 4, margin = 2,
      drop = TRUE
    ) %>% analyze(),
    results = rbind(
      slice_analysis_1, slice_analysis_2, slice_analysis_3,
      slice_analysis_4
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("splitting with tidy eval", {
  skip_on_cran()
  s <- 4
  out <- drake_plan(
    large_data = get_data(),
    slice_analysis = target(
      large_data %>%
        analyze(),
      transform = split(large_data, slices = !!s)
    ),
    results = target(
      rbind(slice_analysis),
      transform = combine(slice_analysis)
    )
  )
  exp <- drake_plan(
    large_data = get_data(),
    slice_analysis_1 = drake_slice(
      data = large_data, slices = 4, index = 1) %>% analyze(),
    slice_analysis_2 = drake_slice(
      data = large_data, slices = 4, index = 2) %>% analyze(),
    slice_analysis_3 = drake_slice(
      data = large_data, slices = 4, index = 3) %>% analyze(),
    slice_analysis_4 = drake_slice(
      data = large_data, slices = 4, index = 4) %>% analyze(),
    results = rbind(
      slice_analysis_1, slice_analysis_2, slice_analysis_3,
      slice_analysis_4
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("parse long tidyeval inputs", {
  skip_on_cran()
  l <- lapply(100, function(x) rep("a", x))
  out <- drake_plan(
    a = target(
      length(x),
      transform = map(x = !!l, y = !!seq_along(l), .id = y)
    )
  )
  exp <- drake_plan(
    a_1L = length(
      c(
        "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a",
        "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a",
        "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a",
        "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a",
        "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a",
        "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a",
        "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a",
        "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a",
        "a", "a", "a", "a", "a"
      )
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("transform_plan() on its own", {
  skip_on_cran()
  i <- 0L
  out1 <- drake_plan(
    y = target(
      f(x, !!i),
      transform = map(x = c(1, 2))
    ),
    transform = FALSE,
    tidy_eval = FALSE
  )
  out2 <- drake_plan(
    z = target(
      g(y, !!i),
      transform = map(y, .id = x)
    ),
    transform = FALSE,
    tidy_eval = FALSE
  )
  out3 <- transform_plan(bind_plans(out1, out2))
  exp1 <- drake_plan(
    y = target(
      command = f(x, !!i),
      transform = map(x = c(1, 2))
    ),
    transform = FALSE
  )
  exp2 <- drake_plan(
    z = target(
      command = g(y, !!i),
      transform = map(y, .id = x)
    ),
    transform = FALSE
  )
  exp3 <- drake_plan(
    y_1 = f(1, 0L),
    y_2 = f(2, 0L),
    z_1 = g(y_1, 0L),
    z_2 = g(y_2, 0L)
  )
  equivalent_plans(out3, exp3)
})

test_with_dir("splice_args()", {
  skip_on_cran()
  out <- splice_args(
    quote(1 + g(f(h(y), z), z)),
    list(y = list(1, 2), z = list(4, quote(x)))
  )
  expect_equal(deparse(out), "1 + g(f(h(1, 2), 4, x), 4, x)")
  out <- splice_args(
    quote(f(x, 5)),
    list(x = list(a = 1, b = quote(sym), c = "char"))
  )
  expect_equal(deparse(out), "f(a = 1, b = sym, c = \"char\", 5)")
})

test_with_dir("make_unique()", {
  skip_on_cran()
  expect_equal(make_unique(character(0)), character(0))
  expect_equal(make_unique(letters), letters)
  x <- c("d", "c", "b", "b", "b", "d", "a", "a", "c", "d")
  out <- make_unique(x)
  exp <- c("d", "c", "b", "b_2", "b_3", "d_2", "a", "a_2", "c_2", "d_3")
  expect_equal(out, exp)
  set.seed(0)
  x <- sample(letters[seq_len(4)], size = 1e3, replace = TRUE)
  out <- make_unique(x)
  out <- vapply(
    out,
    function(x) {
      y <- strsplit(x, split = "_")[[1]]
      if (length(y) == 1L) {
        return(y)
      }
      suffix <- as.integer(y[2])
      expect_true(suffix > 1L)
      paste(y[1], suffix - 1L, sep = ".")
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
  exp <- make.unique(x, sep = ".")
  expect_equal(out, exp)
})

test_with_dir("slice_indices", {
  skip_on_cran()
  all_slices <- function(length, slices) {
    lapply(seq_len(slices), slice_indices, length = length, slices = slices)
  }
  for (i in seq_len(50)) {
    for (j in seq_len(i + 2)) {
      s <- all_slices(i, j)
      expect_equal(sort(unlist(s)), seq_len(i))
      lengths <- vapply(s, length, FUN.VALUE = integer(1))
      diff <- max(lengths) - min(lengths)
      expect_true(diff <= 1L)
    }
  }
})

test_with_dir("slice_indices edge cases", {
  skip_on_cran()
  expect_equal(slice_indices(100, slices = 1, index = 1), seq_len(100))
  expect_equal(slice_indices(100, slices = 1, index = 2), integer(0))
  expect_equal(slice_indices(100, slices = 2, index = 3), integer(0))
  grid <- expand.grid(
    i = c(0L, 100L),
    j = c(0L, 1L),
    k = c(0L, 1L)
  )
  grid <- grid[!(grid$i > 0L & grid$j > 0L & grid$k > 0L), ]
  apply(grid, 1, function(x) {
    expect_equal(
      slice_indices(x["i"], slices = x["j"], index = x["k"]),
      integer(0)
    )
  })
})

test_with_dir("drake_slice edge cases", {
  skip_on_cran()
  expect_error(
    drake_slice(mtcars, margin = 1:2),
    regexp = "must each have length 1"
  )
})

test_with_dir("drake_slice on a vector", {
  skip_on_cran()
  expect_equal(drake_slice(letters, slices = 3, index = 1), letters[1:9])
  expect_equal(drake_slice(letters, slices = 3, index = 2), letters[10:18])
  expect_equal(drake_slice(letters, slices = 3, index = 3), letters[19:26])
})

test_with_dir("drake_slice on a list", {
  skip_on_cran()
  x <- as.list(letters)
  expect_equal(drake_slice(x, slices = 3, index = 1), x[1:9])
  expect_equal(drake_slice(x, slices = 3, index = 2), x[10:18])
  expect_equal(drake_slice(x, slices = 3, index = 3), x[19:26])
})

test_with_dir("drake_slice on arrays", {
  skip_on_cran()
  skip_if_not_installed("abind")
  for (ndim in 1:4) {
    dim <- seq(from = 8, length.out = ndim)
    x <- array(seq_len(prod(dim)), dim = dim)
    for (slices in c(3, 4, 5)) {
      for (margin in seq_len(ndim)) {
        lst <- lapply(
          seq_len(slices),
          function(i) {
            drake_slice(data = x, slices = slices, margin = margin, index = i)
          }
        )
        lst$along <- margin
        # unfixable partial arg match warnings:
        out <- suppressWarnings(do.call(abind::abind, lst))
        expect_equivalent(x, out)
      }
    }
  }
})

test_with_dir("drake_slice on a data frame", {
  skip_on_cran()
  lst <- lapply(
    seq_len(4),
    function(i) {
      drake_slice(data = mtcars, slices = 4, margin = 1, index = i)
    }
  )
  out <- do.call(rbind, lst)
  expect_equal(out, mtcars)
})

test_with_dir("drake_slice and drop", {
  skip_on_cran()
  x <- matrix(seq_len(20), nrow = 5)
  out <- drake_slice(x, slices = 3, margin = 2, index = 2)
  expect_equal(out, matrix(11:15, ncol = 1))
  out <- drake_slice(x, slices = 3, margin = 2, index = 2, drop = TRUE)
  expect_equal(out, 11:15)
})

test_with_dir("slice(), grouping vars, and .id (#963)", {
  skip_on_cran()
  x <- data.frame(
    var = letters[5:8],
    id = letters[1:4],
    stringsAsFactors = FALSE
  )
  out <- drake_plan(
    y = target(
      f(x),
      transform = split(x, slices = !!nrow(x), id = !!x$id, .id = id)
    )
  )
  exp <- drake_plan(
    y_a = f(drake_slice(data = x, slices = 4L, index = 1)),
    y_b = f(drake_slice(data = x, slices = 4L, index = 2)),
    y_c = f(drake_slice(data = x, slices = 4L, index = 3)),
    y_d = f(drake_slice(data = x, slices = 4L, index = 4))
  )
  equivalent_plans(out, exp)
})

test_with_dir("complete_cases()", {
  skip_on_cran()
  for (empty in list(data.frame(), mtcars[NULL, ], mtcars[, NULL])) {
    expect_equivalent(complete_cases(empty), logical(0))
  }
  x <- data.frame(a = letters, b = LETTERS)
  expect_equal(complete_cases(x), rep(TRUE, length(letters)))
  x <- data.frame(a = 1:6, b = c(1:3, rep(NA_integer_, 3)))
  expect_equal(complete_cases(x), rep(c(TRUE, FALSE), each = 3))
})

test_with_dir("side-by-side map keeps grouping vars (#983)", {
  skip_on_cran()
  out <- drake_plan(
    trace = TRUE,
    data = target(simulate(nrow), transform = map(nrow = c(5, 10))),
    data2 = target(simulate(ncol), transform = map(ncol = c(51, 101))),
    data3 = target(somefun(data, data2), transform = map(data, data2)),
  )
  exp <- drake_plan(
    data_5 = target(
      command = simulate(5),
      nrow = "5",
      data = "data_5"
    ),
    data_10 = target(
      command = simulate(10),
      nrow = "10",
      data = "data_10"
    ),
    data2_51 = target(
      command = simulate(51),
      ncol = "51",
      data2 = "data2_51"
    ),
    data2_101 = target(
      command = simulate(101),
      ncol = "101",
      data2 = "data2_101"
    ),
    data3_data_10_data2_101 = target(
      command = somefun(data_10, data2_101),
      nrow = "10",
      data = "data_10",
      ncol = "101",
      data2 = "data2_101",
      data3 = "data3_data_10_data2_101"
    ),
    data3_data_5_data2_51 = target(
      command = somefun(data_5, data2_51),
      nrow = "5",
      data = "data_5",
      ncol = "51",
      data2 = "data2_51",
      data3 = "data3_data_5_data2_51"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("side-by-side cross keeps grouping vars (#983)", {
  skip_on_cran()
  out <- drake_plan(
    trace = TRUE,
    data = target(simulate(nrow), transform = map(nrow = c(5, 10))),
    data2 = target(simulate(ncol), transform = map(ncol = c(51, 101))),
    data3 = target(somefun(data, data2), transform = cross(data, data2)),
  )
  exp <- drake_plan(
    data_5 = target(
      command = simulate(5),
      nrow = "5",
      data = "data_5"
    ),
    data_10 = target(
      command = simulate(10),
      nrow = "10",
      data = "data_10"
    ),
    data2_51 = target(
      command = simulate(51),
      ncol = "51",
      data2 = "data2_51"
    ),
    data2_101 = target(
      command = simulate(101),
      ncol = "101",
      data2 = "data2_101"
    ),
    data3_data_5_data2_101 = target(
      command = somefun(data_5, data2_101),
      nrow = "5",
      data = "data_5",
      ncol = "101",
      data2 = "data2_101",
      data3 = "data3_data_5_data2_101"
    ),
    data3_data_10_data2_101 = target(
      command = somefun(data_10, data2_101),
      nrow = "10",
      data = "data_10",
      ncol = "101",
      data2 = "data2_101",
      data3 = "data3_data_10_data2_101"
    ),
    data3_data_5_data2_51 = target(
      command = somefun(data_5, data2_51),
      nrow = "5",
      data = "data_5",
      ncol = "51",
      data2 = "data2_51",
      data3 = "data3_data_5_data2_51"
    ),
    data3_data_10_data2_51 = target(
      command = somefun(data_10, data2_51),
      nrow = "10",
      data = "data_10",
      ncol = "51",
      data2 = "data2_51",
      data3 = "data3_data_10_data2_51"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("side-by-side cross of nested vars (#983)", {
  skip_on_cran()
  out <- drake_plan(
    a = target(x, transform = map(x = c(1, 1), y = c(3, 3))),
    b = target(a, transform = map(a)),
    c = target(b, transform = map(b)),
    d = target(list(b, c), transform = cross(b, c))
  )
  exp <- drake_plan(
    a_1_3 = 1,
    a_1_3_2 = 1,
    b_a_1_3 = a_1_3,
    b_a_1_3_2 = a_1_3_2,
    c_b_a_1_3 = b_a_1_3,
    c_b_a_1_3_2 = b_a_1_3_2,
    d_b_a_1_3_c_b_a_1_3 = list(b_a_1_3, c_b_a_1_3),
    d_b_a_1_3_2_c_b_a_1_3 = list(b_a_1_3_2, c_b_a_1_3),
    d_b_a_1_3_c_b_a_1_3_2 = list(b_a_1_3, c_b_a_1_3_2),
    d_b_a_1_3_2_c_b_a_1_3_2 = list(b_a_1_3_2, c_b_a_1_3_2)
  )
  equivalent_plans(out, exp)
})

test_with_dir("cross finds the correct combinations (#986)", {
  out <- drake_plan(
    radar = target(
      get_radar_info(radar),
      transform = map(radar = c("a", "b"))
    ),
    set = target(
      get_data(year, month),
      transform = cross(year = c(2015, 2016), month = c(9, 10))
    ),
    cut = target(
      some_crop_function(set, radar),
      transform = cross(radar, set)
    )
  )
  exp <- drake_plan(
    radar_a = get_radar_info("a"),
    radar_b = get_radar_info("b"),
    set_2015_9 = get_data(2015, 9),
    set_2016_9 = get_data(2016, 9),
    set_2015_10 = get_data(2015, 10),
    set_2016_10 = get_data(2016, 10),
    cut_radar_a_set_2015_9 = some_crop_function(set_2015_9, radar_a),
    cut_radar_b_set_2015_9 = some_crop_function(set_2015_9, radar_b),
    cut_radar_a_set_2016_9 = some_crop_function(set_2016_9, radar_a),
    cut_radar_b_set_2016_9 = some_crop_function(set_2016_9, radar_b),
    cut_radar_a_set_2015_10 = some_crop_function(set_2015_10, radar_a),
    cut_radar_b_set_2015_10 = some_crop_function(set_2015_10, radar_b),
    cut_radar_a_set_2016_10 = some_crop_function(set_2016_10, radar_a),
    cut_radar_b_set_2016_10 = some_crop_function(set_2016_10, radar_b)
  )
  equivalent_plans(out, exp)
})

test_with_dir("transform is a formal arg of target() (#993)", {
  skip_on_cran()
  out <- drake_plan(
    radar = target(
      get_radar_info(radar),
      map(radar = c("a", "b"))
    )
  )
  exp <- drake_plan(
    radar_a = get_radar_info("a"),
    radar_b = get_radar_info("b")
  )
  equivalent_plans(out, exp)
  out <- drake_plan(
    radar = target(
      get_radar_info(radar),
      map(radar = c("a", "b")),
      a = 1
    )
  )
  exp <- drake_plan(
    radar_a = target(
      command = get_radar_info("a"),
      a = 1
    ),
    radar_b = target(
      command = get_radar_info("b"),
      a = 1
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("max_expand thins consistently (#1002)", {
  skip_on_cran()
  fns <- letters[seq_len(6)]
  plan <- drake_plan(
    print_fn = target(
      print(fn),
      transform = map(fn = !!fns, .id = FALSE)
    )
  )
  sub2 <- drake_plan(
    print_fn = target(
      print(fn),
      transform = map(fn = !!fns, .id = FALSE)
    ),
    max_expand = 2
  )
  sub3 <- drake_plan(
    print_fn = target(
      print(fn),
      transform = map(fn = !!fns, .id = FALSE)
    ),
    max_expand = 3
  )
  equivalent_plans(sub2, sub3[-2, ])
  equivalent_plans(plan[c(1, 6), ], sub2)
  equivalent_plans(plan[c(1, 6), ], sub2)
})

test_with_dir("max_expand works on split()", {
  skip_on_cran()
  out <- drake_plan(
    analysis = target(
      analyze(data),
      transform = split(data, slices = 10L, margin = 1L, drop = FALSE)
    ),
    max_expand = 2
  )
  exp <- drake_plan(
    analysis_1 = analyze(drake_slice(
      data = data, slices = 10L, index = 1, margin = 1L,
      drop = FALSE
    )),
    analysis_10 = analyze(drake_slice(
      data = data, slices = 10L, index = 10, margin = 1L,
      drop = FALSE
    ))
  )
  equivalent_plans(out, exp)
})

test_with_dir("eliminate partial tagalong grouping vars (#1009)", {
  skip_on_cran()
  m <- c(8L, 9L)
  radars <- c("a", "b")
  out <- drake_plan(
    dataEPLRadar = target(
      command = st_crop(dataEPL, dataRadar),
      transform = cross(dataRadar, dataEPL, .id = c(radar, month))
    ),
    dataEPL = target(
      command = geplr(month),
      transform = cross(month = !!m)
    ),
    dataESL = target(
      command = get_ecmwf_single_level_request(month),
      transform = cross(month = !!m)
    ),
    dataESLRadar = target(
      command = st_crop(dataESL, dataRadar),
      transform = cross(dataRadar, dataESL, .id = c(radar, month))
    ),
    dataRadar = target(
      command = get_radar_info(radar),
      transform = map(radar = !!radars)
    )
  )
  exp <- drake_plan(
    dataEPLRadar_a_8L = st_crop(dataEPL_8L, dataRadar_a),
    dataEPLRadar_b_8L = st_crop(dataEPL_8L, dataRadar_b),
    dataEPLRadar_a_9L = st_crop(dataEPL_9L, dataRadar_a),
    dataEPLRadar_b_9L = st_crop(dataEPL_9L, dataRadar_b),
    dataEPL_8L = geplr(8L),
    dataEPL_9L = geplr(9L),
    dataESL_8L = get_ecmwf_single_level_request(8L),
    dataESL_9L = get_ecmwf_single_level_request(9L),
    dataESLRadar_a_8L = st_crop(dataESL_8L, dataRadar_a),
    dataESLRadar_b_8L = st_crop(dataESL_8L, dataRadar_b),
    dataESLRadar_a_9L = st_crop(dataESL_9L, dataRadar_a),
    dataESLRadar_b_9L = st_crop(dataESL_9L, dataRadar_b),
    dataRadar_a = get_radar_info("a"),
    dataRadar_b = get_radar_info("b")
  )
  equivalent_plans(out, exp)
})

test_with_dir("keep nested grouping vars in combine() (#1008)", {
  skip_on_cran()
  out <- drake_plan(
    i = target(p, transform = map(p = !!(1:2))),
    a = target(x * i, transform = cross(i, x = !!(1:2))),
    b = target(a * y, transform = cross(a, y = !!(1:2), .id = c(p, x))),
    d0 = target(c(b), transform = combine(b, .by = c(a))),
    trace = TRUE
  )
  exp <- drake_plan(
    i_1L = target(
      command = 1L,
      p = "1L",
      i = "i_1L"
    ),
    i_2L = target(
      command = 2L,
      p = "2L",
      i = "i_2L"
    ),
    a_1L_i_1L = target(
      command = 1L * i_1L,
      p = "1L",
      i = "i_1L",
      x = "1L",
      a = "a_1L_i_1L"
    ),
    a_2L_i_1L = target(
      command = 2L * i_1L,
      p = "1L",
      i = "i_1L",
      x = "2L",
      a = "a_2L_i_1L"
    ),
    a_1L_i_2L = target(
      command = 1L * i_2L,
      p = "2L",
      i = "i_2L",
      x = "1L",
      a = "a_1L_i_2L"
    ),
    a_2L_i_2L = target(
      command = 2L * i_2L,
      p = "2L",
      i = "i_2L",
      x = "2L",
      a = "a_2L_i_2L"
    ),
    b_1L_1L = target(
      command = a_1L_i_1L * 1L,
      p = "1L",
      i = "i_1L",
      x = "1L",
      a = "a_1L_i_1L",
      y = "1L",
      b = "b_1L_1L"
    ),
    b_1L_1L_2 = target(
      command = a_1L_i_1L * 2L,
      p = "1L",
      i = "i_1L",
      x = "1L",
      a = "a_1L_i_1L",
      y = "2L",
      b = "b_1L_1L_2"
    ),
    b_1L_2L = target(
      command = a_2L_i_1L * 1L,
      p = "1L",
      i = "i_1L",
      x = "2L",
      a = "a_2L_i_1L",
      y = "1L",
      b = "b_1L_2L"
    ),
    b_1L_2L_2 = target(
      command = a_2L_i_1L * 2L,
      p = "1L",
      i = "i_1L",
      x = "2L",
      a = "a_2L_i_1L",
      y = "2L",
      b = "b_1L_2L_2"
    ),
    b_2L_1L = target(
      command = a_1L_i_2L * 1L,
      p = "2L",
      i = "i_2L",
      x = "1L",
      a = "a_1L_i_2L",
      y = "1L",
      b = "b_2L_1L"
    ),
    b_2L_1L_2 = target(
      command = a_1L_i_2L * 2L,
      p = "2L",
      i = "i_2L",
      x = "1L",
      a = "a_1L_i_2L",
      y = "2L",
      b = "b_2L_1L_2"
    ),
    b_2L_2L = target(
      command = a_2L_i_2L * 1L,
      p = "2L",
      i = "i_2L",
      x = "2L",
      a = "a_2L_i_2L",
      y = "1L",
      b = "b_2L_2L"
    ),
    b_2L_2L_2 = target(
      command = a_2L_i_2L * 2L,
      p = "2L",
      i = "i_2L",
      x = "2L",
      a = "a_2L_i_2L",
      y = "2L",
      b = "b_2L_2L_2"
    ),
    d0_a_1L_i_1L = target(
      command = c(b_1L_1L, b_1L_1L_2),
      p = "1L",
      i = "i_1L",
      x = "1L",
      a = "a_1L_i_1L",
      d0 = "d0_a_1L_i_1L"
    ),
    d0_a_1L_i_2L = target(
      command = c(b_2L_1L, b_2L_1L_2),
      p = "2L",
      i = "i_2L",
      x = "1L",
      a = "a_1L_i_2L",
      d0 = "d0_a_1L_i_2L"
    ),
    d0_a_2L_i_1L = target(
      command = c(b_1L_2L, b_1L_2L_2),
      p = "1L",
      i = "i_1L",
      x = "2L",
      a = "a_2L_i_1L",
      d0 = "d0_a_2L_i_1L"
    ),
    d0_a_2L_i_2L = target(
      command = c(b_2L_2L, b_2L_2L_2),
      p = "2L",
      i = "i_2L",
      x = "2L",
      a = "a_2L_i_2L",
      d0 = "d0_a_2L_i_2L"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("NAs removed from old grouping vars grid (#1010)", {
  skip_on_cran()
  cvo <- c("a3", "7")
  out <- drake_plan(
    data = target(
      command = crossValOmit(radar, crossValOmission),
      transform = cross(
        radar = !!"dd",
        crossValOmission = !!cvo,
        .id = c(radar, crossValOmission)
      )
    ),
    br = target(
      command = annotate_model(data),
      transform = combine(data, .by = data)
    ),
    b = target(
      command = list(crossValId, data),
      transform = cross(
        data,
        crossValId = !!1,
        .id = c(radar, crossValOmission, crossValId)
      )
    ),
    a = target(
      command = list(b),
      transform = combine(b, .by = data)
    ),
    dataTrainList = target(
      command = list2(a, data),
      transform = map(a, data, .id = c(crossValOmission, radar)
      )
    ),
    dataTestList = target(
      command = list(a, data),
      transform = map(a, data, .id = c(crossValOmission, radar)
      )
    )
  )
  exp <- drake_plan(
    data_dd_a3 = crossValOmit("dd", "a3"),
    data_dd_7 = crossValOmit("dd", "7"),
    br_data_dd_7 = annotate_model(data_dd_7),
    br_data_dd_a3 = annotate_model(data_dd_a3),
    b_dd_a3_1 = list(1, data_dd_a3),
    b_dd_7_1 = list(1, data_dd_7),
    a_data_dd_7 = list(b_dd_7_1),
    a_data_dd_a3 = list(b_dd_a3_1),
    dataTrainList_7_dd = list2(a_data_dd_7, data_dd_7),
    dataTrainList_a3_dd = list2(a_data_dd_a3, data_dd_a3),
    dataTestList_7_dd = list(a_data_dd_7, data_dd_7),
    dataTestList_a3_dd = list(a_data_dd_a3, data_dd_a3)
  )
  equivalent_plans(out, exp)
})

test_with_dir("static transforms use only upstream part of plan (#1199)", {
  skip_on_cran()
  radars <- c("radar1", "radar2")
  seasons <- c("season1", "season2")
  months <- c(1, 2)
  radar_seasons <- expand.grid(
    radar = radars,
    season = seasons,
    stringsAsFactors = FALSE
  )
  out <- drake_plan(
    data = target(
      get_data(radar, month),
      transform = cross(radar = !!radars, month = !!months)
    ),
    to_cross = target(
      list(data),
      transform = combine(data, .by = radar)
    ),
    problem = target(
      list(to_cross, season),
      transform = cross(to_cross, season = !!seasons)
    ),
    separate = target(
      list(radar, season),
      transform = map(.data = !!radar_seasons)
    ),
    trace = TRUE
  )
  exp <- drake_plan(
    data_radar1_1 = target(
      command = get_data("radar1", 1),
      radar = "\"radar1\"",
      month = "1",
      data = "data_radar1_1"
    ),
    data_radar2_1 = target(
      command = get_data("radar2", 1),
      radar = "\"radar2\"",
      month = "1",
      data = "data_radar2_1"
    ),
    data_radar1_2 = target(
      command = get_data("radar1", 2),
      radar = "\"radar1\"",
      month = "2",
      data = "data_radar1_2"
    ),
    data_radar2_2 = target(
      command = get_data("radar2", 2),
      radar = "\"radar2\"",
      month = "2",
      data = "data_radar2_2"
    ),
    problem_season1_to_cross_radar1 = target(
      command = list(to_cross_radar1, "season1"),
      radar = "\"radar1\"",
      season = "\"season1\"",
      separate = "separate_radar1_season1",
      to_cross = "to_cross_radar1",
      problem = "problem_season1_to_cross_radar1"
    ),
    problem_season2_to_cross_radar1 = target(
      command = list(to_cross_radar1, "season2"),
      radar = "\"radar1\"",
      season = "\"season2\"",
      separate = "separate_radar1_season2",
      to_cross = "to_cross_radar1",
      problem = "problem_season2_to_cross_radar1"
    ),
    problem_season1_to_cross_radar2 = target(
      command = list(to_cross_radar2, "season1"),
      radar = "\"radar1\"",
      season = "\"season1\"",
      separate = "separate_radar1_season1",
      to_cross = "to_cross_radar2",
      problem = "problem_season1_to_cross_radar2"
    ),
    problem_season2_to_cross_radar2 = target(
      command = list(to_cross_radar2, "season2"),
      radar = "\"radar1\"",
      season = "\"season2\"",
      separate = "separate_radar1_season2",
      to_cross = "to_cross_radar2",
      problem = "problem_season2_to_cross_radar2"
    ),
    separate_radar1_season1 = target(
      command = list("radar1", "season1"),
      radar = "\"radar1\"",
      season = "\"season1\"",
      separate = "separate_radar1_season1"
    ),
    separate_radar2_season1 = target(
      command = list("radar2", "season1"),
      radar = "\"radar2\"",
      season = "\"season1\"",
      separate = "separate_radar2_season1"
    ),
    separate_radar1_season2 = target(
      command = list("radar1", "season2"),
      radar = "\"radar1\"",
      season = "\"season2\"",
      separate = "separate_radar1_season2"
    ),
    separate_radar2_season2 = target(
      command = list("radar2", "season2"),
      radar = "\"radar2\"",
      season = "\"season2\"",
      separate = "separate_radar2_season2"
    ),
    to_cross_radar1 = target(
      command = list(data_radar1_1, data_radar1_2),
      radar = "\"radar1\"",
      to_cross = "to_cross_radar1"
    ),
    to_cross_radar2 = target(
      command = list(data_radar2_1, data_radar2_2),
      radar = "\"radar2\"",
      to_cross = "to_cross_radar2"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("custom names (#1240)", {
  out <- drake_plan(
    x = target(
      f(x),
      transform = map(x = !!seq_len(2), .names = c("a", "b"))
    ),
    y = target(
      f(w, x),
      transform = cross(
        w = !!seq_len(2),
        x,
        .names = c("aa", "ab", "ba", "bb")
      )
    ),
    z = target(
      g(y),
      transform = map(y)
    ),
    final = target(
      h(z),
      transform = combine(z, .by = x, .names = c("final1", "final2"))
    )
  )
  exp <- drake_plan(
    final1 = h(z_aa, z_ab),
    final2 = h(z_ba, z_bb),
    a = f(1L),
    b = f(2L),
    aa = f(1L, a),
    ab = f(2L, a),
    ba = f(1L, b),
    bb = f(2L, b),
    z_aa = g(aa),
    z_ab = g(ab),
    z_ba = g(ba),
    z_bb = g(bb)
  )
  equivalent_plans(out, exp)
})

test_with_dir("User-defined splot() names (#1240)", {
  skip_on_cran()
  out <- drake_plan(
    large_data = get_data(),
    slice_analysis = target(
      large_data %>%
        analyze(),
      transform = split(large_data, slices = 4, .names = letters[seq_len(4)])
    ),
    results = target(
      rbind(slice_analysis),
      transform = combine(slice_analysis)
    )
  )
  exp <- drake_plan(
    large_data = get_data(),
    a = drake_slice(
      data = large_data, slices = 4, index = 1) %>% analyze(),
    b = drake_slice(
      data = large_data, slices = 4, index = 2) %>% analyze(),
    c = drake_slice(
      data = large_data, slices = 4, index = 3) %>% analyze(),
    d = drake_slice(
      data = large_data, slices = 4, index = 4) %>% analyze(),
    results = rbind(a, b, c, d)
  )
  equivalent_plans(out, exp)
})

test_with_dir("custom names of bad length (#1240)", {
  expect_error(
    drake_plan(
      x = target(
        f(x),
        transform = map(x = !!seq_len(2), .names = "a")
      )
    ),
    regexp = "same length"
  )
})

test_with_dir("splicing confusion with formal arguments of c() (#1262)", {
  out <- drake_plan(
    a = target(
      lapply(seq_len(b), sqrt),
      transform = map(b = c(1, 2))
    ),
    d = target(
      unlist(a, use.names = FALSE),
      transform = combine(a)
    )
  )
  exp <- drake_plan(
    a_1 = lapply(seq_len(1), sqrt),
    a_2 = lapply(seq_len(2), sqrt),
    d = unlist(a_1, a_2, use.names = FALSE)
  )
  equivalent_plans(out, exp)
})
