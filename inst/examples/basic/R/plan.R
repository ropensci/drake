# This is where you set up your workflow plan,
# a data frame with the steps of your analysis.

# We write drake commands to generate our two bootstrapped datasets.
my_datasets <- drake_plan(
  small = simulate(48),
  large = simulate(64)
)

# Optionally, get replicates with expand(my_datasets,
#   values = c("rep1", "rep2")).
# Bootstrapping involves randomness, so this is good practice
# in real life. But this is a miniaturized workflow,
# so we will not use replicates here.

# This is a wildcard template for generating more commands.
# These new commands will apply our regression models
# to each of the datasets in turn.
methods <- drake_plan(
  regression1 = reg1(dataset__),
  regression2 = reg2(dataset__)
)

# Here, we use the template to expand the `methods` template
# over the datasets we will analyze.
# Same as evaluate(methods, wildcard = "..dataset..",
#   values = my_datasets$target)
my_analyses <- plan_analyses(methods, datasets = my_datasets)

# Now, we summarize each regression fit of each bootstrapped dataset.
# We will look at these summaries to figure out if fuel efficiency
# and weight are related somehow.
# Again, this is a template. Later we will expand it over the
# available regression models.
summary_types <- drake_plan(
  summ = suppressWarnings(summary(analysis__$residuals)), # Summarize the RESIDUALS of the model fit. # nolint
  coef = suppressWarnings(summary(analysis__))$coefficients # Coefficinents with p-values # nolint
)

# Here, we expand the commands to summarize each analysis in turn.
# summaries() also uses evaluate(): once with expand = TRUE,
#   once with expand = FALSE
results <- plan_summaries(
  summary_types,
  my_analyses,
  my_datasets,
  gather = NULL
) # skip 'gather' (workflow my_plan is more readable)

# External file targets and dependencies should be single-quoted.
# Use double quotes to remove any special meaning from character strings.
# Single quotes inside imported functions are ignored, so this mechanism
# only works inside the workflow my_plan data frame.
# WARNING: drake cannot track entire directories (folders).
report <- drake_plan(
  # As long as `knit()`, `knitr::knit()`, `render()`, or `rmarkdown::render()`
  # is visible in your workflow plan command,
  # drake will dig into the active code chunks of your `report.Rmd`
  # and find the dependencies of `report.md` in the arguments of
  # calls to loadd() and readd().
  report.md = knit(
    'report.Rmd', #nolint: use single quotes to specify file dependency.
     quiet = TRUE
  ),
  file_targets = TRUE,
  strings_in_dots = "filenames" # Redundant, since we used single quotes
)

# Row order doesn't matter in the workflow my_plan.
my_plan <- rbind(report, my_datasets, my_analyses, results)
