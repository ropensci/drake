# This is where you set up your workflow plan,
# a data frame with the steps of your analysis.

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

# Put together the whole plan.
whole_plan <- rbind(model_plan, rmspe_plan, rmspe_results_plan, output_plan)
