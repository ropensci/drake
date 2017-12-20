# This analysis explores some trends in R package downloads over time.
# The data change often, but drake makes sure
# the project does not restart from scratch.
#
# Also see the example-packages.Rmd vignette,
# https://github.com/wlandau-lilly/drake/blob/master/vignettes/example-packages.Rmd

library(drake)
library(cranlogs)
library(knitr)
library(ggplot2)
library(plyr)

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
  recent = cran_downloads(packages = package_list, when = "last-month"),
  older = cran_downloads(
    packages = package_list,
    from = "2016-11-01",
    to = "2016-12-01"
  ),
  strings_in_dots = "literals"
)

# We want to summarize each set of
# the download statistics a couple different ways.

output_types <- drake_plan(
  averages = make_my_table(dataset__),
  plot = make_my_plot(dataset__)
)

# We need to define functions to summarize
# and plot the data.

make_my_table <- function(downloads){
  ddply(downloads, "package", function(package_downloads){
    data.frame(mean_downloads = mean(package_downloads$count))
  })
}

make_my_plot <- function(downloads){
  ggplot(downloads) +
    geom_line(aes(x = date, y = count, group = package, color = package))
}

# Below, the symbols `recent` and `older`
# each substitute in for the `dataset__` wildcard.
# Thus, `output_plan` has four rows.

output_plan <- plan_analyses(
  plan = output_types,
  datasets = data_plan
)

# We plan to weave the results together
# in a dynamic knitr report.

report_plan <- drake_plan(
  report.md = knit("report.Rmd", quiet = TRUE),
  file_targets = TRUE
)

# And we complete the workflow plan data frame by
# concatenating the results together.

whole_plan <- rbind(
  data_plan,
  output_plan,
  report_plan
)

# The most recent downloads are from the last month,
# so the data need constant updating.
# We use triggers to make sure the recent data is always downloaded
# on every make().

whole_plan$trigger = "any"
whole_plan$trigger[whole_plan$target == "recent"] = "always"

# Now, we run the project to download the data and analyze it.
# The results will be summarized in the knitted report, `report.md`,
# but you can also read the results directly from the cache.

make(whole_plan)
readd(averages_recent)
readd(plot_recent)

# Because we used triggers, each make() rebuilds the `recent`
# data frame to get the latest download numbers for today.
# If the data did not change from last time, nothing else
# needs to be built.

make(whole_plan)

# If you rerun the project tomorrow,
# the download counts will have been updated, so make()
# will refresh `averages_recent`, `plot_recent`, and
# `'report.md'`. Targets `averages_older` and `plot_older`
# are unaffected, so drake will skip them.
