drake_context("plan transformations")

test_with_dir("simple expansion", {
  plan <- drake_plan(a = target(1 + 1, transform = cross(x = c(1, 2))))
  expect_equal(sort(plan$target), sort(c("a_1", "a_2")))
  expect_equal(plan$command, rep("1 + 1", 2))
})

test_with_dir("transforming the mtcars plan", {
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
      transform = summarize(data, sum_fun)
    ),
    others = target(
      analyze(list(c(summ), c(data))),
      transform = summarize(data, sum_fun)
    ),
    final_winner = target(
      min(winners),
      transform = summarize()
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
      summ_coef_reg_reg1_large = summ_coef_reg_reg1_large,
      summ_coef_reg_reg2_large = summ_coef_reg_reg2_large
    ),
    winners_small_coef = min(
      summ_coef_reg_reg1_small = summ_coef_reg_reg1_small,
      summ_coef_reg_reg2_small = summ_coef_reg_reg2_small
    ),
    winners_large_residuals = min(
      summ_residuals_reg_reg1_large = summ_residuals_reg_reg1_large,
      summ_residuals_reg_reg2_large = summ_residuals_reg_reg2_large
    ),
    winners_small_residuals = min(
      summ_residuals_reg_reg1_small = summ_residuals_reg_reg1_small,
      summ_residuals_reg_reg2_small = summ_residuals_reg_reg2_small
    ),
    others_large_coef = analyze(list(
      c(
        summ_coef_reg_reg1_large = summ_coef_reg_reg1_large,
        summ_coef_reg_reg2_large = summ_coef_reg_reg2_large
      ),
      c(large = large)
    )),
    others_small_coef = analyze(list(
      c(
        summ_coef_reg_reg1_small = summ_coef_reg_reg1_small,
        summ_coef_reg_reg2_small = summ_coef_reg_reg2_small
      ),
      c(small = small)
    )),
    others_large_residuals = analyze(list(
      c(
        summ_residuals_reg_reg1_large = summ_residuals_reg_reg1_large,
        summ_residuals_reg_reg2_large = summ_residuals_reg_reg2_large
      ),
      c(large = large)
    )),
    others_small_residuals = analyze(list(
      c(
        summ_residuals_reg_reg1_small = summ_residuals_reg_reg1_small,
        summ_residuals_reg_reg2_small = summ_residuals_reg_reg2_small
      ),
      c(small = small)
    )),
    final_winner = min(
      winners_large_coef = winners_large_coef,
      winners_small_coef = winners_small_coef,
      winners_large_residuals = winners_large_residuals,
      winners_small_residuals = winners_small_residuals
    )
  )
  out <- out[order(out$target), ]
  exp <- exp[order(exp$target), ]
  expect_equal(out$target, exp$target)
  expect_equal(
    lapply(out$command, standardize_command),
    lapply(exp$command, standardize_command)
  )
})

test_with_dir("transformations and custom columns", {
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
        transform = summarize(data, sum_fun),
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
