drake_context("dsl")

test_with_dir("simple expansion", {
  plan <- drake_plan(a = target(1 + 1, transform = cross(x = c(1, 2))))
  expect_equal(sort(plan$target), sort(c("a_1", "a_2")))
  expect_equal(plan$command, rep("1 + 1", 2))
})

test_with_dir("dsl with different types", {
  plan <- drake_plan(
    a = target(1 + 1, transform = cross(x = c(1, 2))),
    transform = FALSE
  )
  plan$command <- list(quote(1 + 1))
  plan <- transform_plan(plan)
  expect_equal(sort(plan$target), sort(c("a_1", "a_2")))
  expect_equal(plan$command, rep("1 + 1", 2))
})

test_with_dir("dsl with the mtcars plan", {
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
      transform = reduce(data, sum_fun)
    ),
    others = target(
      analyze(list(c(summ), c(data))),
      transform = reduce(data, sum_fun)
    ),
    final_winner = target(
      min(winners),
      transform = reduce()
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
    winners_large_coef = min(list(
      summ_coef_reg_reg1_large = summ_coef_reg_reg1_large,
      summ_coef_reg_reg2_large = summ_coef_reg_reg2_large
    )),
    winners_small_coef = min(list(
      summ_coef_reg_reg1_small = summ_coef_reg_reg1_small,
      summ_coef_reg_reg2_small = summ_coef_reg_reg2_small
    )),
    winners_large_residuals = min(list(
      summ_residuals_reg_reg1_large = summ_residuals_reg_reg1_large,
      summ_residuals_reg_reg2_large = summ_residuals_reg_reg2_large
    )),
    winners_small_residuals = min(list(
      summ_residuals_reg_reg1_small = summ_residuals_reg_reg1_small,
      summ_residuals_reg_reg2_small = summ_residuals_reg_reg2_small
    )),
    others_large_coef = analyze(list(
      c(list(
        summ_coef_reg_reg1_large = summ_coef_reg_reg1_large,
        summ_coef_reg_reg2_large = summ_coef_reg_reg2_large
      )),
      c(list(large = large))
    )),
    others_small_coef = analyze(list(
      c(list(
        summ_coef_reg_reg1_small = summ_coef_reg_reg1_small,
        summ_coef_reg_reg2_small = summ_coef_reg_reg2_small
      )),
      c(list(small = small))
    )),
    others_large_residuals = analyze(list(
      c(list(
        summ_residuals_reg_reg1_large = summ_residuals_reg_reg1_large,
        summ_residuals_reg_reg2_large = summ_residuals_reg_reg2_large
      )),
      c(list(large = large))
    )),
    others_small_residuals = analyze(list(
      c(list(
        summ_residuals_reg_reg1_small = summ_residuals_reg_reg1_small,
        summ_residuals_reg_reg2_small = summ_residuals_reg_reg2_small
      )),
      c(list(small = small))
    )),
    final_winner = min(list(
      winners_large_coef = winners_large_coef,
      winners_small_coef = winners_small_coef,
      winners_large_residuals = winners_large_residuals,
      winners_small_residuals = winners_small_residuals
    ))
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
        transform = reduce(data, sum_fun),
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
    quote(target(simulate(48), data = 123)),
    quote(target(simulate(48), reg = 123)),
    quote(target(simulate(48), reg_fun = 123)),
    quote(target(simulate(48), sum_fun = 123)),
    quote(target(simulate(48), summ = 123))
  )
  msg <- "cannot also be custom column names in the plan"
  lapply(illegals, function(illegal) {
    e[[2]] <- illegal
    expect_error(eval(e), regexp = msg)
  })
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
      transform = reduce(data, sum_fun)
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
      transform = reduce(data, sum_fun)
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

test_with_dir("running a dsl-generated mtcars plan", {
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

test_with_dir("dsl groupings", {
  out <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg1 = target(
      rgfun(data),
      transform = cross(data = c(small, large)),
      group = c(reg, othergroup)
    ),
    reg2 = target(
      rgfun(data),
      transform = cross(data = c(small, large)),
      group = reg
    ),
    winners = target(
      min(reg),
      transform = reduce(data),
      a = 1
    ),
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
    reg2_large = target(
      command = rgfun(large),
      data = "large",
      reg = "reg2_large",
      reg2 = "reg2_large"
    ),
    reg2_small = target(
      command = rgfun(small),
      data = "small",
      reg = "reg2_small",
      reg2 = "reg2_small"
    ),
    winners_large = target(
      command = min(reg1_large = reg1_large, reg2_large = reg2_large),
      a = 1,
      data = "large",
      winners = "winners_large"
    ),
    winners_small = target(
      command = min(reg1_small = reg1_small, reg2_small = reg2_small),
      a = 1,
      data = "small",
      winners = "winners_small"
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
      transform = cross(data = c(small, large)),
      group = reg
    ),
    reg2 = target(
      reg_fun(data),
      transform = cross(data = c(small, large)),
      group = reg
    ),
    winners = target(
      min(reg),
      transform = reduce(data),
      a = 1
    ),
    transform = FALSE
  )
  expect_equal(
    sort(out$target),
    sort(c("small", "large", "reg1", "reg2", "winners"))
  )
})

test_with_dir("dsl within quotes", {
  plan1 <- drake_plan(
    analysis = target(
      analyze_data("source"),
      transform = cross(source = c(source1, source2))
    ),
    transform = FALSE
  )
  plan2 <- drake_plan(
    reduce = target(
      reduce_analyses(analysis),
      transform = reduce()
    ),
    transform = FALSE
  )
  plan <- bind_plans(plan1, plan2)
  plan
  out <- transform_plan(plan)
  exp <- drake_plan(
    analysis_source1 = analyze_data("source1"),
    analysis_source2 = analyze_data("source2"),
    reduce = reduce_analyses(
      analysis_source1 = analysis_source1,
      analysis_source2 = analysis_source2
    )
  )
  equivalent_plans(out, exp)
  out <- transform_plan(plan, trace = TRUE)
  exp <- drake_plan(
    analysis_source1 = target(
      command = analyze_data("source1"),
      source = "source1",
      analysis = "analysis_source1"
    ),
    analysis_source2 = target(
      command = analyze_data("source2"),
      source = "source2",
      analysis = "analysis_source2"
    ),
    reduce = target(
      command = reduce_analyses(
        analysis_source1 = analysis_source1,
        analysis_source2 = analysis_source2
      ),
      reduce = "reduce"
    )
  )
  expect_true(ncol(exp) > 2)
  equivalent_plans(out, exp)
})
