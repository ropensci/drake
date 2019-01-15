drake_context("transform")

test_with_dir("transformations of plans", {
  out <- drake_plan(
    small = simulate(48),
    large = simulate(64),
    reg = target(
      reg_fun(data),
      transform = cross(reg_fun = c(reg1, reg2), data = c(small, large))
    ),
    summary = target(
      sum_fun(data, reg),
      transform = cross(sum_fun = c(coefficients, residuals), reg)
    ),
    winners = target(
      min(summary),
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
    summary_coefficients_reg_reg1_large = coefficients(large, reg_reg1_large),
    summary_residuals_reg_reg1_large = residuals(large, reg_reg1_large),
    summary_coefficients_reg_reg1_small = coefficients(small, reg_reg1_small),
    summary_residuals_reg_reg1_small = residuals(small, reg_reg1_small),
    summary_coefficients_reg_reg2_large = coefficients(large, reg_reg2_large),
    summary_residuals_reg_reg2_large = residuals(large, reg_reg2_large),
    summary_coefficients_reg_reg2_small = coefficients(small, reg_reg2_small),
    summary_residuals_reg_reg2_small = residuals(small, reg_reg2_small),
    winners_large_coefficients = min(
      summary_coefficients_reg_reg1_large =
        summary_coefficients_reg_reg1_large,
      summary_coefficients_reg_reg2_large =
        summary_coefficients_reg_reg2_large
    ),
    winners_small_coefficients = min(
      summary_coefficients_reg_reg1_small =
        summary_coefficients_reg_reg1_small,
      summary_coefficients_reg_reg2_small =summary_coefficients_reg_reg2_small
    ),
    winners_large_residuals = min(
      summary_residuals_reg_reg1_large = summary_residuals_reg_reg1_large,
      summary_residuals_reg_reg2_large = summary_residuals_reg_reg2_large
    ),
    winners_small_residuals = min(
      summary_residuals_reg_reg1_small = summary_residuals_reg_reg1_small,
      summary_residuals_reg_reg2_small = summary_residuals_reg_reg2_small
    )
  )
  expect_equivalent(out[order(out$target), ], exp[order(exp$target), ])
})
