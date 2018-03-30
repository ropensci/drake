# This is where you set up your workflow plan,
# a data frame with the steps of your analysis.

# We want to explore the daily downloads from these packages.

package_list <- c(
  "knitr",
  "Rcpp",
  "ggplot2"
)

# We plan to use the cranlogs package.
# The data frames `older` and `recent` will
# contain the number of daily downloads for each package
# from the RStudio CRAN mirror.

data_plan <- drake_plan(
  older = cran_downloads(
    packages = package_list,
    from = "2016-11-01",
    to = "2016-12-01"
  ),
  recent = target(
    command = cran_downloads(
      packages = package_list,
      when = "last-month"
    ),
    trigger = "always"
  ),
  strings_in_dots = "literals"
)

# The latest download data needs to be refreshed every day,
# so in data_plan above, we use triggers
# to force `recent` to always build.
# For more on triggers, see the vignette on debugging and testing:
# https://ropensci.github.io/drake/articles/debug.htmll#test-with-triggers- # nolint

# We want to summarize each set of
# download statistics a couple different ways.

output_types <- drake_plan(
  averages = make_my_table(dataset__),
  plot = make_my_plot(dataset__)
)

# Below, the targets `recent` and `older`
# each take turns substituting the `dataset__` wildcard.
# Thus, `output_plan` has four rows.

output_plan <- plan_analyses(
  plan = output_types,
  datasets = data_plan
)

# We plan to weave the results together
# in a dynamic knitr report.

report_plan <- drake_plan(
  knit(knitr_in("report.Rmd"), file_out("report.md"), quiet = TRUE)
)

# And we complete the workflow plan data frame by
# concatenating the results together.
# Drake analyzes the plan to figure out the dependency network,
# so row order does not matter.

whole_plan <- bind_plans(
  data_plan,
  output_plan,
  report_plan
)
