drake_context("dsl")

test_with_dir("empty transforms", {
  out <- drake_plan(
    a = target(x, transform = cross()),
    b = target(y, transform = combine()),
    c = target(z, transform = map())
  )
  equivalent_plans(out, transform_plan(out, envir = environment()))
  exp <- drake_plan(a = x, b = y, c = z)
  equivalent_plans(out, exp)
})

test_with_dir("more empty transforms", {
  x_vals <- NULL
  out <- drake_plan(a = target(x, transform = map(x = !!x_vals)))
  exp <- drake_plan(a = x)
  equivalent_plans(out, exp)
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
    command = c("z", "")
  )
  equivalent_plans(out, exp)
})

test_with_dir("bad transform", {
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
    a_1.1 = target(
      command = 1,
      x = "1",
      a = "a_1.1"
    ),
    b_a_1 = target(
      command = f(a_1),
      x = "1",
      a = "a_1",
      b = "b_a_1"
    ),
    b_a_1.1 = target(
      command = f(a_1.1),
      x = "1",
      a = "a_1.1",
      b = "b_a_1.1"
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
    trace = T
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
  out <- drake_plan(
    x = target(
      y,
      transform = cross(
        x = c(1, 2),
        .tag_in = c(one, second)
      )
    ),
    trace = T
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
  out <- drake_plan(
    x = target(
      y,
      transform = cross(
        x = c(1, 2),
        .tag_out = single
      )
    ),
    trace = T
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
  out <- drake_plan(
    x = target(
      y,
      transform = cross(
        x = c(1, 2),
        .tag_out = c(one, second)
      )
    ),
    trace = T
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
  out <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    lots = target(nobody(home), transform = cross(a, b)),
    mots = target(everyone(out), transform = map(c, d)),
    winners = target(min(nobodyhome), transform = combine(data = list()))
  )
  exp <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    lots = nobody(home),
    mots = everyone(out),
    winners = min(nobodyhome)
  )
  equivalent_plans(out, exp)
})

test_with_dir("command symbols are for combine() but the plan has them", {
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
    data_group2_.example1.com. = pull_data("example1.com"),
    data_group2_.example2.com. = pull_data("example2.com"),
    larger = bind_rows(
      data_group1_1_3, data_group1_2_4,
      data_group2_.example1.com., data_group2_.example2.com. # nolint
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
  plan <- transform_plan(plan, envir = environment())
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
      transform = combine(summ = list(), .by = c(data, sum_fun))
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
    winners_data_48_coef = min(list(
      summ_coef_reg_reg1_data_48,
      summ_coef_reg_reg2_data_48
    )),
    winners_data_64_coef = min(list(
      summ_coef_reg_reg1_data_64,
      summ_coef_reg_reg2_data_64
    )),
    winners_data_48_resid = min(list(
      summ_resid_reg_reg1_data_48,
      summ_resid_reg_reg2_data_48
    )),
    winners_data_64_resid = min(list(
      summ_resid_reg_reg1_data_64,
      summ_resid_reg_reg2_data_64
    ))
  )
  equivalent_plans(out, exp)
})

test_with_dir("map with unequal columns", {
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
        transform = combine(summ = list(), .by = c(data, sum_fun)),
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
      transform = combine(data = list(), sum_fun = list())
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
      transform = combine(data = list(), sum_fun = list())
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
    winners = target(min(reg), transform = combine(reg = list()), a = 1),
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
      command = min(list(
        reg1_small,
        reg1_large,
        reg2_small,
        reg2_large
      )),
      a = 1,
      winners = "winners"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("combine() and tags", {
  i <- as.numeric(1:3)
  out <- drake_plan(
    x = target(1, transform = map(f = !!i, .tag_in = grp, .tag_out = targs)),
    y = target(1, transform = map(g = !!i, .tag_in = grp, .tag_out = targs)),
    z = target(
      min(targs),
      transform = combine(
        targs = list(),
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
      command = min(list(x_1, x_2, x_3)),
      grp = "x",
      z = "z_x",
      im = "z",
      here = "z_x"
    ),
    z_y = target(
      command = min(list(y_1, y_2, y_3)),
      grp = "y",
      z = "z_y",
      im = "z",
      here = "z_y"
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("can disable transformations in dsl", {
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
      transform = combine(data = list()),
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
      transform = combine(analysis = list())
    ),
    transform = FALSE
  )
  plan <- bind_plans(plan1, plan2)
  out <- transform_plan(plan, envir = environment())
  exp <- drake_plan(
    analysis_.source1. = analyze_data("source1"), # nolint
    analysis_source2 = analyze_data(source2),
    analysis_3 = analyze_data(3),
    reducks = combine_analyses(list(
      analysis_.source1., # nolint
      analysis_source2,
      analysis_3
    ))
  )
  equivalent_plans(out, exp)
  out <- transform_plan(plan, envir = environment(), trace = TRUE)
  exp <- drake_plan(
    analysis_.source1. = target( # nolint
      command = analyze_data("source1"),
      source = "\"source1\"",
      analysis = "analysis_.source1."
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
      command = combine_analyses(list(
        analysis_.source1., # nolint
        analysis_source2,
        analysis_3
      )),
      reducks = "reducks"
    )
  )
  expect_true(ncol(exp) > 2)
  equivalent_plans(out, exp)
})

test_with_dir("tidy eval in the DSL", {
  sms <- rlang::syms(letters)
  out <- drake_plan(
    x = target(
      f(char),
      trigger = trigger(condition = g(char)),
      custom = h(char),
      transform = map(char = !!sms)
    )
  )
  exp <- drake_plan(
    x_a = target(
      command = f(a),
      trigger = trigger(
        condition = g(a)
      ),
      custom = h(a)
    ),
    x_b = target(
      command = f(b),
      trigger = trigger(
        condition = g(b)
      ),
      custom = h(b)
    ),
    x_c = target(
      command = f(c),
      trigger = trigger(
        condition = g(c)
      ),
      custom = h(c)
    ),
    x_d = target(
      command = f(d),
      trigger = trigger(
        condition = g(d)
      ),
      custom = h(d)
    ),
    x_e = target(
      command = f(e),
      trigger = trigger(
        condition = g(e)
      ),
      custom = h(e)
    ),
    x_f = target(
      command = f(f),
      trigger = trigger(
        condition = g(f)
      ),
      custom = h(f)
    ),
    x_g = target(
      command = f(g),
      trigger = trigger(
        condition = g(g)
      ),
      custom = h(g)
    ),
    x_h = target(
      command = f(h),
      trigger = trigger(
        condition = g(h)
      ),
      custom = h(h)
    ),
    x_i = target(
      command = f(i),
      trigger = trigger(
        condition = g(i)
      ),
      custom = h(i)
    ),
    x_j = target(
      command = f(j),
      trigger = trigger(
        condition = g(j)
      ),
      custom = h(j)
    ),
    x_k = target(
      command = f(k),
      trigger = trigger(
        condition = g(k)
      ),
      custom = h(k)
    ),
    x_l = target(
      command = f(l),
      trigger = trigger(
        condition = g(l)
      ),
      custom = h(l)
    ),
    x_m = target(
      command = f(m),
      trigger = trigger(
        condition = g(m)
      ),
      custom = h(m)
    ),
    x_n = target(
      command = f(n),
      trigger = trigger(
        condition = g(n)
      ),
      custom = h(n)
    ),
    x_o = target(
      command = f(o),
      trigger = trigger(
        condition = g(o)
      ),
      custom = h(o)
    ),
    x_p = target(
      command = f(p),
      trigger = trigger(
        condition = g(p)
      ),
      custom = h(p)
    ),
    x_q = target(
      command = f(q),
      trigger = trigger(
        condition = g(q)
      ),
      custom = h(q)
    ),
    x_r = target(
      command = f(r),
      trigger = trigger(
        condition = g(r)
      ),
      custom = h(r)
    ),
    x_s = target(
      command = f(s),
      trigger = trigger(
        condition = g(s)
      ),
      custom = h(s)
    ),
    x_t = target(
      command = f(t),
      trigger = trigger(
        condition = g(t)
      ),
      custom = h(t)
    ),
    x_u = target(
      command = f(u),
      trigger = trigger(
        condition = g(u)
      ),
      custom = h(u)
    ),
    x_v = target(
      command = f(v),
      trigger = trigger(
        condition = g(v)
      ),
      custom = h(v)
    ),
    x_w = target(
      command = f(w),
      trigger = trigger(
        condition = g(w)
      ),
      custom = h(w)
    ),
    x_x = target(
      command = f(x),
      trigger = trigger(
        condition = g(x)
      ),
      custom = h(x)
    ),
    x_y = target(
      command = f(y),
      trigger = trigger(
        condition = g(y)
      ),
      custom = h(y)
    ),
    x_z = target(
      command = f(z),
      trigger = trigger(
        condition = g(z)
      ),
      custom = h(z)
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("dsl: exact same plan as mtcars", {
  skip_if_not_installed("knitr")
  out <- drake_plan(
    report = knit(knitr_in("report.Rmd"), file_out("report.md"), quiet = TRUE),
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
      transform = combine(data_sim = list(), .by = local)
    )
  )
  exp <- drake_plan(
    data_sim_1_3 = sim_data(mean = 1, sd = 3),
    data_sim_2_3 = sim_data(mean = 2, sd = 3),
    data_sim_1_4 = sim_data(mean = 1, sd = 4),
    data_sim_2_4 = sim_data(mean = 2, sd = 4),
    data_download_.http...url_1. = download_data(url = "http://url_1"),
    data_download_.http...url_2. = download_data(url = "http://url_2"),
    data_pkg_.gapminder. = load_data_from_package(pkg = "gapminder"),
    data_pkg_.Ecdat. = load_data_from_package(pkg = "Ecdat"),
    summaries_data_sim_1_3 = compare_ds(list(data_sim_1_3)),
    summaries_data_sim_1_4 = compare_ds(list(data_sim_1_4)),
    summaries_data_sim_2_3 = compare_ds(list(data_sim_2_3)),
    summaries_data_sim_2_4 = compare_ds(list(data_sim_2_4))
  )
  equivalent_plans(out, exp)
})

test_with_dir("trace has correct provenance", {
  out <- drake_plan(
    trace = TRUE,
    a = target(x, transform = map(x = c(1, 1), y = c(3, 3))),
    b = target(a, transform = map(a)),
    c = target(b, transform = map(b)),
    d = target(b, transform = cross(b, c)),
    e = target(c, transform = map(c)),
    f = target(c, transform = map(c)),
    g = target(b, transform = map(b)),
    h = target(a, transform = map(a)),
    i = target(e, transform = combine(e = list())),
    j = target(f, transform = combine(f = list()))
  )
  exp <- drake_plan(
    a_1_3 = target(
      command = 1,
      x = "1",
      y = "3",
      a = "a_1_3"
    ),
    a_1_3.1 = target(
      command = 1,
      x = "1",
      y = "3",
      a = "a_1_3.1"
    ),
    b_a_1_3 = target(
      command = a_1_3,
      x = "1",
      y = "3",
      a = "a_1_3",
      b = "b_a_1_3"
    ),
    b_a_1_3.1 = target(
      command = a_1_3.1,
      x = "1",
      y = "3",
      a = "a_1_3.1",
      b = "b_a_1_3.1"
    ),
    c_b_a_1_3 = target(
      command = b_a_1_3,
      x = "1",
      y = "3",
      a = "a_1_3",
      b = "b_a_1_3",
      c = "c_b_a_1_3"
    ),
    c_b_a_1_3.1 = target(
      command = b_a_1_3.1,
      x = "1",
      y = "3",
      a = "a_1_3.1",
      b = "b_a_1_3.1",
      c = "c_b_a_1_3.1"
    ),
    d_b_a_1_3_c_b_a_1_3 = target(
      command = b_a_1_3,
      x = "1",
      y = "3",
      a = "a_1_3",
      b = "b_a_1_3",
      c = "c_b_a_1_3",
      d = "d_b_a_1_3_c_b_a_1_3"
    ),
    d_b_a_1_3_c_b_a_1_3.1 = target(
      command = b_a_1_3,
      b = "b_a_1_3",
      c = "c_b_a_1_3.1",
      d = "d_b_a_1_3_c_b_a_1_3.1"
    ),
    d_b_a_1_3.1_c_b_a_1_3 = target(
      command = b_a_1_3.1,
      b = "b_a_1_3.1",
      c = "c_b_a_1_3",
      d = "d_b_a_1_3.1_c_b_a_1_3"
    ),
    d_b_a_1_3.1_c_b_a_1_3.1 = target(
      command = b_a_1_3.1,
      x = "1",
      y = "3",
      a = "a_1_3.1",
      b = "b_a_1_3.1",
      c = "c_b_a_1_3.1",
      d = "d_b_a_1_3.1_c_b_a_1_3.1"
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
    e_c_b_a_1_3.1 = target(
      command = c_b_a_1_3.1,
      x = "1",
      y = "3",
      a = "a_1_3.1",
      b = "b_a_1_3.1",
      c = "c_b_a_1_3.1",
      e = "e_c_b_a_1_3.1"
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
    f_c_b_a_1_3.1 = target(
      command = c_b_a_1_3.1,
      x = "1",
      y = "3",
      a = "a_1_3.1",
      b = "b_a_1_3.1",
      c = "c_b_a_1_3.1",
      f = "f_c_b_a_1_3.1"
    ),
    g_b_a_1_3 = target(
      command = b_a_1_3,
      x = "1",
      y = "3",
      a = "a_1_3",
      b = "b_a_1_3",
      g = "g_b_a_1_3"
    ),
    g_b_a_1_3.1 = target(
      command = b_a_1_3.1,
      x = "1",
      y = "3",
      a = "a_1_3.1",
      b = "b_a_1_3.1",
      g = "g_b_a_1_3.1"
    ),
    h_a_1_3 = target(
      command = a_1_3,
      x = "1",
      y = "3",
      a = "a_1_3",
      h = "h_a_1_3"
    ),
    h_a_1_3.1 = target(
      command = a_1_3.1,
      x = "1",
      y = "3",
      a = "a_1_3.1",
      h = "h_a_1_3.1"
    ),
    i = target(
      command = list(e_c_b_a_1_3, e_c_b_a_1_3.1),
      i = "i"
    ),
    j = target(
      command = list(f_c_b_a_1_3, f_c_b_a_1_3.1),
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
      transform = combine(summ = list(), .by = c(data, sum_fun))
    ),
    others = target(
      analyze(list(c(summ), c(data))),
      transform = combine(
        summ = list(),
        data = list(),
        .by = c(data, sum_fun)
      )
    ),
    final_winner = target(
      min(winners),
      transform = combine(winners = list())
    )
  )
  plan2 <- drake_plan(
    final_winner = target(
      min(winners),
      transform = combine(winners = list())
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
        summ = list(),
        data = list(),
        .by = c(data, sum_fun)
      )
    ),
    winners = target(
      min(summ),
      transform = combine(summ = list(), .by = c(data, sum_fun))
    ),
    large = simulate(64)
  )
  expect_equal(nrow(plan1), 23L)
  equivalent_plans(plan1, plan2)
})

test_with_dir("gh #696", {
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
    splits_.lf1.txt. = {
      file_in("lf1.txt")
      file_out(c("lf1.txtaa", "lf1.txtab", "lf1.txtac"))
      system2("split", c(paste0("-n r/", 3), "lf1.txt", "lf1.txt"))
    },
    splits_.lf2.txt. = {
      file_in("lf2.txt")
      file_out(c("lf2.txtaa", "lf2.txtab", "lf2.txtac"))
      system2("split", c(paste0("-n r/", 3), "lf2.txt", "lf2.txt"))
    }
  )
  equivalent_plans(out, exp)
})

test_with_dir("transformations in triggers", {
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
      transform = combine(summ = list(), .by = c(data, sum_fun))
    ),
    others = target(
      analyze(list(c(summ), c(data))),
      trigger = trigger(change = analyze(list(c(summ), c(data)))),
      transform = combine(
        summ = list(),
        data = list(),
        .by = c(data, sum_fun)
      )
    ),
    final_winner = target(
      min(winners),
      trigger = trigger(change = min(winners)),
      transform = combine(winners = list())
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
        list(
          summ_coef_reg_reg1_large,
          summ_coef_reg_reg2_large
        )
      ),
      trigger = trigger(
        change = min(
          list(
            summ_coef_reg_reg1_large,
            summ_coef_reg_reg2_large
          )
        )
      )
    ),
    winners_small_coef = target(
      command = min(
        list(
          summ_coef_reg_reg1_small,
          summ_coef_reg_reg2_small
        )
      ),
      trigger = trigger(
        change = min(
          list(
            summ_coef_reg_reg1_small,
            summ_coef_reg_reg2_small
          )
        )
      )
    ),
    winners_large_residuals = target(
      command = min(
        list(
          summ_residuals_reg_reg1_large,
          summ_residuals_reg_reg2_large
        )
      ),
      trigger = trigger(
        change = min(
          list(
            summ_residuals_reg_reg1_large,
            summ_residuals_reg_reg2_large
          )
        )
      )
    ),
    winners_small_residuals = target(
      command = min(
        list(
          summ_residuals_reg_reg1_small,
          summ_residuals_reg_reg2_small
        )
      ),
      trigger = trigger(
        change = min(
          list(
            summ_residuals_reg_reg1_small,
            summ_residuals_reg_reg2_small
          )
        )
      )
    ),
    others_large_coef = target(
      command = analyze(list(
        c(list(summ_coef_reg_reg1_large, summ_coef_reg_reg2_large)),
        c(list(large))
      )),
      trigger = trigger(
        change = analyze(list(
          c(list(summ_coef_reg_reg1_large, summ_coef_reg_reg2_large)),
          c(list(large))
        ))
      )
    ),
    others_small_coef = target(
      command = analyze(list(
        c(list(summ_coef_reg_reg1_small, summ_coef_reg_reg2_small)),
        c(list(small))
      )),
      trigger = trigger(
        change = analyze(list(
          c(list(summ_coef_reg_reg1_small, summ_coef_reg_reg2_small)),
          c(list(small))
        ))
      )
    ),
    others_large_residuals = target(
      command = analyze(list(
        c(list(summ_residuals_reg_reg1_large, summ_residuals_reg_reg2_large)),
        c(list(large))
      )),
      trigger = trigger(
        change = analyze(list(
          c(list(summ_residuals_reg_reg1_large, summ_residuals_reg_reg2_large)),
          c(list(large))
        ))
      )
    ),
    others_small_residuals = target(
      command = analyze(list(
        c(list(summ_residuals_reg_reg1_small, summ_residuals_reg_reg2_small)),
        c(list(small))
      )),
      trigger = trigger(
        change = analyze(list(
          c(list(summ_residuals_reg_reg1_small, summ_residuals_reg_reg2_small)),
          c(list(small))
        ))
      )
    ),
    final_winner = target(
      command = min(list(
        winners_large_coef, winners_small_coef, winners_large_residuals,
        winners_small_residuals
      )),
      trigger = trigger(
        change = min(list(
          winners_large_coef, winners_small_coef, winners_large_residuals,
          winners_small_residuals
        ))
      )
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir(".id = FALSE", {
  x_ <- letters[1:2]
  y_ <- letters[3:4]
  z_ <- letters[11:14]
  out <- drake_plan(
    a = target(c(x, y), transform = cross(x = !!x_, y = !!y_, .id = FALSE)),
    b = target(c(a, z), transform = map(a, z = !!z_, .id = FALSE)),
    d = target(b, transform = combine(b = list(), .by = x, .id = FALSE))
  )
  exp <- drake_plan(
    a = c("a", "c"),
    a.1 = c("b", "c"),
    a.2 = c("a", "d"),
    a.3 = c("b", "d"),
    b = c(a, "k"),
    b.1 = c(a.1, "l"),
    b.2 = c(a.2, "m"),
    b.3 = c(a.3, "n"),
    d = list(b, b.2),
    d.1 = list(b.1, b.3)
  )
  equivalent_plans(out, exp)
})

test_with_dir("(1) .id = syms. (2) map() finds the correct cross() syms", {
  x_ <- letters[1:2]
  y_ <- letters[3:4]
  z_ <- letters[11:12]
  out <- drake_plan(
    A = target(
      c(x, y, z),
      transform = cross(x = !!x_, y = !!y_, z = !!z_, .id = z)
    ),
    B = target(c(A, y, z), transform = map(A, y, z, .id = c(y, z))),
    C = target(B, transform = combine(B = list(), .by = c(x, y), .id = bad))
  )
  # nolint start
  exp <- drake_plan(
    A_.k. = c("a", "c", "k"),
    A_.k..1 = c("b", "c", "k"),
    A_.k..2 = c("a", "d", "k"),
    A_.k..3 = c("b", "d", "k"),
    A_.l. = c("a", "c", "l"),
    A_.l..1 = c("b", "c", "l"),
    A_.l..2 = c("a", "d", "l"),
    A_.l..3 = c("b", "d", "l"),
    B_.c._.k. = c(A_.k., "c", "k"),
    B_.c._.k..1 = c(A_.k..1, "c", "k"),
    B_.d._.k. = c(A_.k..2, "d", "k"),
    B_.d._.k..1 = c(A_.k..3, "d", "k"),
    B_.c._.l. = c(A_.l., "c", "l"),
    B_.c._.l..1 = c(A_.l..1, "c", "l"),
    B_.d._.l. = c(A_.l..2, "d", "l"),
    B_.d._.l..1 = c(A_.l..3, "d", "l"),
    C = list(B_.c._.k., B_.c._.l.),
    C.1 = list(B_.c._.k..1, B_.c._.l..1),
    C.2 = list(B_.d._.k., B_.d._.l.),
    C.3 = list(B_.d._.k..1, B_.d._.l..1)
  )
  # nolint end
  equivalent_plans(out, exp)
})

test_with_dir("repeated maps do not duplicate targets", {
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
    A.1 = "a",
    B = c(A, "a"),
    B.1 = c(A.1, "a"),
    C = "b",
    C.1 = "b",
    D = c(A, B, C, "a", "b"),
    D.1 = c(A.1, B.1, C.1, "a", "b")
  )

  equivalent_plans(out, exp)
})

test_with_dir("unequal trace vars are not duplicated in map()", {
  inputs <- lapply(LETTERS[1:4], as.symbol)
  types <- rep(c(1, 2), each = 2)
  out <- drake_plan(
    wide1 = target(
      ez_parallel(a),
      transform = map(a = !!inputs, type = !!types) ),
    prelim = target(
      preliminary(wide1),
      transform = combine(wide1 = list(), .by = type) ),
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
    prelim_1 = preliminary(list(wide1_A_1, wide1_B_1)),
    prelim_2 = preliminary(list(wide1_C_2, wide1_D_2)),
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
    B = target(A, transform = combine(A = list())),
    C = target(c(A), transform = combine(A = list())),
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
    a_1_3_5_.7._a = 1 + f(1, 3, 5, "7", a),
    a_2_4_6_.8._b = 1 + f(2, 4, 6, "8", b)
  )
  equivalent_plans(out, exp)
})

test_with_dir("empty grids", {
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
      transform = map(
        x = c(),
        y = c(),
        .data = !!grid[logical(0), , drop = FALSE] # nolint
      )
    )
  )
  exp <- drake_plan(a = 1 + f(x, y, z, w, v))
  equivalent_plans(out, exp)
})

test_with_dir("grid for GitHub issue 697", {
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
    s_load_.G1._.R1. = load_csv("G1", "R1"),
    s_load_.G2._.R1. = load_csv("G2", "R1"),
    s_load_.G1._.R2. = load_csv("G1", "R2"),
    s_load_.G2._.R2. = load_csv("G2", "R2"),
    s_load_.G1._.R3. = load_csv("G1", "R3"),
    s_load_.G2._.R3. = load_csv("G2", "R3"),
    s_load_.G1._.R4. = load_csv("G1", "R4"),
    s_load_.G2._.R4. = load_csv("G2", "R4"),
    s_load_.G1._.R5. = load_csv("G1", "R5"),
    s_load_.G1._.R6. = load_csv("G1", "R6")
  )
  equivalent_plans(out, exp)
})

test_with_dir("grid for GitHub issue 710", {
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
      transform = combine(wide = list(), .by = type)
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
    serial_1 = expensive_calc(list(wide_A_1, wide_C_1, wide_E_1)),
    serial_2 = expensive_calc(list(wide_B_2, wide_D_2)),
    dist_serial_1_wide_A = distribute_results(serial_1, wide_A),
    dist_serial_2_wide_B = distribute_results(serial_2, wide_B),
    dist_serial_1_wide_C = distribute_results(serial_1, wide_C),
    dist_serial_2_wide_D = distribute_results(serial_2, wide_D),
    dist_serial_1_wide_E = distribute_results(serial_1, wide_E)
  )
  equivalent_plans(out, exp)
})

test_with_dir("combine() with symbols instead of calls", {
  out <- drake_plan(
    data = target(
      get_data(param),
      transform = map(param = c(1, 2))
    ),
    results = target(
      .data %>%
        data,
      transform = combine(data = select)
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
  out <- drake_plan(
    data = target(
      get_data(param),
      transform = map(param = c(1, 2))
    ),
    results = target(
      .data %>%
        c(data, 2, data),
      transform = combine(data = min(0, na.rm = FALSE))
    )
  )
  exp <- drake_plan(
    data_1 = get_data(1),
    data_2 = get_data(2),
    results = .data %>% c(
      min(data_1, data_2, 0, na.rm = FALSE),
      2,
      min(data_1, data_2, 0, na.rm = FALSE)
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("each grouping var in combine() needs a grouping fn", {
  expect_error(
    drake_plan(
      data = target(
        get_data(param),
        transform = map(param = c(1, 2))
      ),
      results = target(
        .data %>%
          data,
        transform = combine(data)
      )
    ),
    regexp = "please supply a grouping function to each grouping variable"
  )
})

test_with_dir("invalid splitting var", {
  out <- drake_plan(
    data = target(x, transform = map(x = c(1, 2)), nothing = NA),
    results = target(
      data,
      transform = combine(data = list(), .by = nothing)
    )
  )
  out <- out[, c("target", "command")]
  exp <- drake_plan(
    data_1 = 1,
    data_2 = 2,
    results = data
  )
  equivalent_plans(out, exp)
})
