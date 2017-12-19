# The following data analysis workflow shows off
# `drake`'s ability to generate lots of reproducibly-tracked
# tasks with ease.
# The same technique would be cumbersome, even intractable,
# with GNU Make (https://www.gnu.org/software/make/).
#
# The goal is to search for factors closely associated with
# the productivity of states in the USA around the 1970s and 1980s.
# For the sake of simplicity, we use gross state product as a metric
# of productivity, and we restrict ourselves to
# multiple linear regression models with three variables.
# For each of the 84 possible models, we fit the data and then
# evaluate the root mean squared prediction error (RMSPE).
#
# RMSPE = sqrt(mean((y - yhat)^2)) # nolint
#
# Here, `y` is the vector of observed gross state products in the data,
# and `yhat` is the vector of predicted gross state products
# under one of the models.
# We take the best variables to be
# the triplet in the model with the lowest RMSPE.
#
# Also see the example-gsp.Rmd vignette:
# https://github.com/wlandau-lilly/drake/blob/master/vignettes/example-gsp.Rmd

library(drake)
library(Ecdat) # econometrics datasets
library(knitr)
library(ggplot2)

data(Produc) # Gross State Product
head(Produc) # ?Produc

# We want to predict "gsp" based on the other variables.
predictors <- setdiff(colnames(Produc), "gsp")

# Try all combinations of three covariates.
combos <- t(combn(predictors, 3))
head(combos)

# Use these combinations to generate
# a workflow plan data frame for drake.
# We generate the plan in stages.

# First, we apply the models to the datasets.
targets <- apply(combos, 1, paste, collapse = "_")

commands <- apply(combos, 1, function(row){
  covariates <- paste(row, collapse = " + ")
  formula <- paste0("as.formula(\"gsp ~ ", covariates, "\")")
  command <- paste0("lm(", formula, ", data = Produc)")
})

model_plan <- data.frame(target = targets, command = commands)

# Judge the models based on the root mean squared prediction error (RMSPE)
commands <- paste0("get_rmspe(", targets, ", data = Produc)")
targets <- paste0("rmspe_", targets)
rmspe_plan <- data.frame(target = targets, command = commands)

# We need to define a function to get the RMSPE.
get_rmspe <- function(lm_fit, data){
  y <- data$gsp
  yhat <- predict(lm_fit, data = data)
  terms <- attr(summary(lm_fit)$terms, "term.labels")
  data.frame(
    rmspe = sqrt(mean((y - yhat)^2)), # nolint
    X1 = terms[1],
    X2 = terms[2],
    X3 = terms[3]
  )
}

# Aggregate all the results together.
rmspe_results_plan <- gather_plan(
  plan = rmspe_plan,
  target = "rmspe",
  gather = "rbind"
)

# Plan some final output.
output_plan <- drake_plan(
  rmspe.pdf = ggsave(filename = "rmspe.pdf", plot = plot_rmspe(rmspe)),
  report.md = knit("report.Rmd", quiet = TRUE),
  file_targets = TRUE,
  strings_in_dots = "literals"
)

# We need a function to generate the plot.
plot_rmspe <- function(rmspe){
  ggplot(rmspe) +
    geom_histogram(aes(x = rmspe), bins = 30)
}

# Put together the whole plan.
whole_plan <- rbind(model_plan, rmspe_plan, rmspe_results_plan, output_plan)

# Optionally, visualize the interactive workflow graph.
# config <- drake_config(whole_plan) # nolint
# vis_drake_graph(config) # nolint

# Just see the dependencies of the report.
# vis_drake_graph(config, from = "'report.md'", mode = "in", order = 1) # nolint

# Run the project.
# View the results rmspe.pdf and report.md
make(whole_plan, jobs = 2, verbose = 3)

# Rendering the final output requires pandoc,
# so I did not include it in the workflow plan.
# rmarkdown::render("report.md") # nolint

# Read the results from the drake cache.
rmspe <- readd(rmspe)

# See the best models. The best variables
# are in the top row under `X1`, `X2`, and `X3`.`
head(rmspe[order(rmspe$rmspe, decreasing = FALSE), ])
