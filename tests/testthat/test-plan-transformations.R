drake_context("plan transformations")

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
    )
  )
  out$command <- lapply(out$command, standardize_command)
  exp$command <- lapply(exp$command, standardize_command)
  expect_equal(out[order(out$target), ], exp[order(exp$target), ])
})
