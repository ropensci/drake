#' @title Load the mtcars example from `drake_example("mtcars")`
#' @description Is there an association between
#' the weight and the fuel efficiency of cars?
#' To find out, we use the mtcars dataset.
#' The mtcars dataset itself only has 32 rows,
#' so we generate two larger bootstrapped datasets
#' and then analyze them with regression models.
#' Finally, we summarize the regression models
#' to see if there is an association.
#' @details Use `drake_example("mtcars")` to get the code
#' for the mtcars example. The included R script is a detailed,
#' heavily-commented walkthrough. The chapter of the user manual at
#' <https://ropenscilabs.github.io/drake-manual/mtcars.html> # nolint
#' also walks through the mtcars example.
#' This function also writes/overwrites
#' the file, `report.Rmd`.
#' @export
#' @return A [drake_config()] configuration list.
#' @inheritParams drake_config
#' @param envir The environment to load the example into.
#'   Defaults to your workspace.
#'   For an insulated workspace,
#'   set `envir = new.env(parent = globalenv())`.
#' @param report_file where to write the report file `report.Rmd`.
#' @param overwrite logical, whether to overwrite an
#'   existing file `report.Rmd`
#' @param force logical, whether to force the loading of a
#'   non-back-compatible cache from a previous version of drake.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # Populate your workspace and write 'report.Rmd'.
#' load_mtcars_example() # Get the code: drake_example("mtcars")
#' # Check the dependencies of an imported function.
#' deps_code(reg1)
#' # Check the dependencies of commands in the workflow plan.
#' deps_code(my_plan$command[1])
#' deps_code(my_plan$command[4])
#' # Plot the interactive network visualization of the workflow.
#' config <- drake_config(my_plan)
#' outdated(config) # Which targets are out of date?
#' # Run the workflow to build all the targets in the plan.
#' make(my_plan)
#' outdated(config) # Everything should be up to date.
#' # For the reg2() model on the small dataset,
#' # the p-value is so small that there may be an association
#' # between weight and fuel efficiency after all.
#' readd(coef_regression2_small)
#' # Remove the whole cache.
#' clean(destroy = TRUE)
#' # Clean up the imported file.
#' unlink("report.Rmd")
#' })
#' }
load_mtcars_example <- function(
  envir = parent.frame(),
  report_file = "report.Rmd",
  overwrite = FALSE,
  force = FALSE
){
  eval(parse(text = "base::require(drake, quietly = TRUE)"))
  eval(parse(text = "base::require(knitr, quietly = TRUE)"))
  mtcars <- get("mtcars")

  # Pick a random subset of n rows from a dataset
  evalq(
    random_rows <- function(data, n){
      data[sample.int(n = nrow(data), size = n, replace = TRUE), ]
    },
    envir = envir
  )

  # Bootstrapped datasets from mtcars.
  evalq(
    simulate <- function(n){
      # Pick a random set of cars to bootstrap from the mtcars data.
      data <- random_rows(data = mtcars, n = n)

      # x is the car's weight, and y is the fuel efficiency.
      data.frame(
        x = data$wt,
        y = data$mpg
      )
    },
    envir = envir
  )

  # Is there a linear relationship between weight and fuel efficiency?
  evalq(
    reg1 <- function(d) {
      lm(y ~ +x, data = d)
    },
    envir = envir
  )

  # Is there a QUADRATIC relationship between weight and fuel efficiency?
  evalq(
    reg2 <- function(d) {
      d$x2 <- d$x ^ 2
      lm(y ~ x2, data = d)
    },
    envir = envir
  )

  # construct workflow plan

  # remove 'undefinded globals' errors in R CMD check
  dataset__ <- analysis__ <- large <- small <-
    simulate <- knit <- my_knit <- report_dependencies <-
    reg1 <- reg2 <- coef_regression2_small <- NULL

  datasets <- drake_plan(small = simulate(48), large = simulate(64))

  methods <- drake_plan(
    regression1 = reg1(dataset__),
    regression2 = reg2(dataset__)
  )

  # Same as evaluate_plan(methods, wildcard = 'dataset__',
  #   values = datasets$output).
  analyses <- plan_analyses(methods, datasets = datasets)

  summary_types <- drake_plan(
    summ = suppressWarnings(summary(analysis__$residuals)), # Summarize the RESIDUALS of the model fit. # nolint
    coef = suppressWarnings(summary(analysis__))$coefficients # Coefficinents with p-values # nolint
  )

  # plan_summaries() also uses evaluate_plan(): once with expand = TRUE,
  # once with expand = FALSE
  # skip 'gather' (drake_plan my_plan is more readable)
  results <- plan_summaries(summary_types, analyses, datasets, gather = NULL)

  report <- tibble(
    target = "report",
    command = 'knit(knitr_in("report.Rmd"), file_out("report.md"), quiet = TRUE)' # nolint  
  )

  # Row order doesn't matter in the drake_plan my_plan.
  envir$my_plan <- rbind(report, datasets, analyses, results)

  # Write the R Markdown source for a dynamic knitr report
  report <- system.file(
    file.path("rmarkdown", "mtcars.Rmd"),
    package = "drake",
    mustWork = TRUE
  )
  if (file.exists(report_file) & overwrite){
    warning("Overwriting file 'report.Rmd'.")
  }
  file.copy(from = report, to = report_file, overwrite = overwrite)
  invisible()
}
