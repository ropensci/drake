# Generate the HTML widgets in the docs/images/ folder.
# These interactive graphs are embedded in the vignettes.
# Requires pandoc.

html_out <- function(...) here::here("docs", "images", ...)

devtools::load_all() # load current drake
clean(destroy = TRUE)
config <- load_basic_example(overwrite = TRUE)

vis_drake_graph(config, file = html_out("outdated.html"), selfcontained = TRUE,
  width = "100%", height = "500px")

config <- make(my_plan)

vis_drake_graph(config, file = html_out("built.html"), selfcontained = TRUE,
  width = "100%", height = "500px")

reg2 <- function(d){
  d$x3 <- d$x ^ 3
  lm(y ~ x3, data = d)
}

vis_drake_graph(config, file = html_out("reg2.html"), selfcontained = TRUE,
  width = "100%", height = "500px")

vis_drake_graph(config, file = html_out("reg2-small-legend.html"), selfcontained = TRUE,
  width = "100%", height = "500px", full_legend = FALSE)

vis_drake_graph(config, file = html_out("reg2-no-legend.html"), selfcontained = TRUE,
  width = "100%", height = "500px", ncol_legend = 0)

vis_drake_graph(
  config, file = html_out("targetsonly.html"), selfcontained = TRUE,
  targets_only = TRUE,
  width = "100%", height = "500px",
  from = c("large", "small")
)

vis_drake_graph(
  config, file = html_out("fromout.html"), selfcontained = TRUE,
  width = "100%", height = "500px",
  from = c("regression2_small", "regression2_large")
)

vis_drake_graph(
  config, file = html_out("fromin.html"), selfcontained = TRUE,
  width = "100%", height = "500px",
  from = "small", mode = "in"
)

vis_drake_graph(
  config, file = html_out("fromall.html"), selfcontained = TRUE,
  width = "100%", height = "500px",
  from = "small", mode = "all", order = 1
)

vis_drake_graph(
  config, file = html_out("subset.html"), selfcontained = TRUE,
  width = "100%", height = "500px",
  subset = c("regression2_small", "\"report.md\"")
)

clean(destroy = TRUE)
unlink("report.Rmd")

# For the "packages" example.

library(magrittr)
reportfile <- file.path("examples", "packages", "report.Rmd") %>%
  system.file(package = "drake", mustWork = TRUE)
file.copy(reportfile, getwd())
runfile <- file.path("examples", "packages", "interactive-tutorial.R") %>%
  system.file(package = "drake", mustWork = TRUE)
source(runfile)
vis_drake_graph(
  config, file = html_out("packages.html"), selfcontained = TRUE,
  width = "100%", height = "500px"
)

# For the best practices vignette
get_data <- function(){
  "Get the data."
}

analyze_data <- function(){
  "Analyze the data."
}

summarize_results <- function(){
  "Summarize the results."
}

files <- c("data.csv", "get_data.R", "analyze_data.R", "summarize_data.R")
for (file in files){
  file.create(file)
}

my_plan <- drake_plan(
  my_data = get_data(file_in("data.csv")), # nolint
  my_analysis = analyze_data(my_data),
  my_summaries = summarize_results(my_data, my_analysis)
)
config <- drake_config(my_plan)
vis_drake_graph(
  main = "Good workflow plan",
  config, file = html_out("good-commands.html"), selfcontained = TRUE,
  width = "100%", height = "500px"
)

my_plan <- drake_plan(
  my_data = source(file_in("get_data.R")), # nolint
  my_analysis = source(file_in("analyze_data.R")), # nolint
  my_summaries = source(file_in("summarize_data.R")) # nolint
)
config <- drake_config(my_plan)
vis_drake_graph(
  main = "Bad workflow plan",
  config, file = html_out("bad-commands.html"), selfcontained = TRUE,
  width = "100%", height = "500px"
)

for (file in files){
  file.remove(file)
}

clean(destroy = TRUE)
unlink(c("figure", "report.Rmd"), recursive = TRUE)

unlink(html_out("*_files"), recursive = TRUE)
