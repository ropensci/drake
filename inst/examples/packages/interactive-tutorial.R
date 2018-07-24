# This file is an interactive tutorial that only depends
# on the included report.Rmd file.
# It is meant to walk you through the analysis step by step.
# The other files show how to set up this example
# as a serious drake project. Run make.R to deploy it
# as a serious workflow.
#
################
### OVERVIEW ###
################
#
# This small data analysis project explores some trends
# in R package downloads over time.
# The datasets are downloaded using the cranlogs package
# (https://github.com/metacran/cranlogs).

library(cranlogs)
cran_downloads(packages = "dplyr", when = "last-week")

# Above, each count is the number of times `dplyr`
# was downloaded from the RStudio CRAN mirror on the given day.
# To stay up to date with the latest download statistics,
# we need to refresh the data frequently.
# With `drake`, we can bring all our work up to date
# without restarting everything from scratch.
#
# This example is paired with a chapter of the user manual:
# https://ropenscilabs.github.io/drake-manual/example-packages.html

################
### ANALYSIS ###
################

# Drake knows about the packages you load with library() or require().

library(cranlogs)
library(drake)
library(dplyr)
library(ggplot2)
library(knitr)
library(rvest)

pkgconfig::set_config("drake::strings_in_dots" = "literals")

# We want to explore the daily downloads from these packages.

package_list <- c(
  "knitr",
  "Rcpp",
  "ggplot2"
)

# We will use the cranlogs package to get the data.
# The data frames `older` and `recent` will
# contain the number of daily downloads for each package
# from the RStudio CRAN mirror.
# For the recent data, we will use a custom trigger.
# That way, drake automatically knows to fetch the recent data
# when a new CRAN log becomes available.

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
    trigger = trigger(change = latest_log_date())
  )
)

# To find out the latest log date,
# we scrape the web with the rvest package.

latest_log_date <- function(){
  read_html("http://cran-logs.rstudio.com/") %>%
    html_nodes("li:last-of-type") %>%
    html_nodes("a:last-of-type") %>%
    html_text() %>%
    max
}

# We want to summarize each set of
# download statistics a couple different ways.

output_types <- drake_plan(
  averages = make_my_table(dataset__),
  plot = make_my_plot(dataset__)
)

# We need to define functions to summarize
# and plot the data.

make_my_table <- function(downloads){
  group_by(downloads, package) %>%
    summarize(mean_downloads = mean(count))
}

make_my_plot <- function(downloads){
  ggplot(downloads) +
    geom_line(aes(x = date, y = count, group = package, color = package))
}

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
  report = knit(knitr_in("report.Rmd"), file_out("report.md"), quiet = TRUE)
)

# And we complete the workflow plan data frame by
# concatenating the results together.
# Drake analyzes the plan to figure out the dependency network,
# so row order does not matter.

whole_plan <- dplyr::bind_rows(
  data_plan,
  output_plan,
  report_plan
)

# Now, we run the project to download the data and analyze it.
# The results will be summarized in the knitted report, `report.md`,
# but you can also read the results directly from the cache.

make(whole_plan)
readd(averages_recent)
readd(plot_recent)

# Now, subsequent calls to make()
# will report that everything is up to date
# unless a new log has been uploaded to http://cran-logs.rstudio.com/
# since last time.

make(whole_plan)

# To visualize the build behavior, plot the dependency graph.

config <- drake_config(whole_plan)
vis_drake_graph(config)
