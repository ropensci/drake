# Generate the HTML widgets in the images/ folder.
# These interactive graphs are embedded in the vignettes.
# Requires pandoc.
devtools::load_all() # load current drake
clean(destroy = TRUE)
config <- load_basic_example()

vis_drake_graph(config, file = "outdated.html", selfcontained = TRUE,
  width = "100%", height = "500px")

config <- make(my_plan)

vis_drake_graph(config, file = "built.html", selfcontained = TRUE,
  width = "100%", height = "500px")

reg2 <- function(d){
  d$x3 <- d$x ^ 3
  lm(y ~ x3, data = d)
}

vis_drake_graph(config, file = "reg2.html", selfcontained = TRUE,
  width = "100%", height = "500px")


vis_drake_graph(
  config, file = "targetsonly.html", selfcontained = TRUE,
  targets_only = TRUE,
  width = "100%", height = "500px",
  from = c("large", "small")
)

vis_drake_graph(
  config, file = "fromout.html", selfcontained = TRUE,
  width = "100%", height = "500px",
  from = c("regression2_small", "regression2_large")
)

vis_drake_graph(
  config, file = "fromin.html", selfcontained = TRUE,
  width = "100%", height = "500px",
  from = "small", mode = "in"
)

vis_drake_graph(
  config, file = "fromall.html", selfcontained = TRUE,
  width = "100%", height = "500px",
  from = "small", mode = "all", order = 1
)

vis_drake_graph(
  config, file = "subset.html", selfcontained = TRUE,
  width = "100%", height = "500px",
  subset = c("regression2_small", "'report.md'")
)

clean(destroy = TRUE)
unlink("report.Rmd")

# For the "packages" example.

library(magrittr)
reportfile <- file.path("examples", "packages", "report.Rmd") %>%
  system.file(package = "drake", mustWork = TRUE)
file.copy(reportfile, getwd())
runfile <- file.path("examples", "packages", "run.R") %>%
  system.file(package = "drake", mustWork = TRUE)
source(runfile)
vis_drake_graph(
  config, file = "packages.html", selfcontained = TRUE,
  width = "100%", height = "500px"
)

clean(destroy = TRUE)
unlink(c("figure", "report.Rmd"), recursive = TRUE)
