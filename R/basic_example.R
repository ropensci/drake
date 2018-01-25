#' @title Load the basic example from \code{drake_example("basic")}
#' @description Is there an association between
#' the weight and the fuel efficiency of cars?
#' To find out, we use the mtcars dataset.
#' The mtcars dataset itself only has 32 rows,
#' so we generate two larger bootstrapped datasets
#' and then analyze them with regression models.
#' Finally, we summarize the regression models
#' to see if there is an association.
#' @details Use \code{\link{drake_example}('basic')} to get the code
#' for the basic example. The included R script is a detailed,
#' heavily-commented walkthrough. The quickstart vignette at
#' \url{https://github.com/ropensci/drake/blob/master/vignettes/quickstart.Rmd} # nolint
#' and \url{https://ropensci.github.io/drake/articles/quickstart.html}
#' also walks through the basic example.
#' This function also writes/overwrites
#' the file, \code{report.Rmd}.
#' @export
#' @return A \code{\link{drake_config}()} configuration list.
#' @param envir The environment to load the example into.
#' Defaults to your workspace.
#' For an insulated workspace,
#' set \code{envir = new.env(parent = globalenv())}.
#' @param cache Optional \code{storr} cache to use.
#' @param report_file where to write the report file \code{report.Rmd}.
#' @param to deprecated, where to write the dynamic report source file
#' \code{report.Rmd}
#' @param overwrite logical, whether to overwrite an
#' existing file \code{report.Rmd}
#' @param verbose logical, whether to print console messages.
#' @param force logical, whether to force the loading of a
#' non-back-compatible cache from a previous version of drake.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # Populate your workspace and write 'report.Rmd'.
#' load_basic_example() # Get the code: drake_example("basic")
#' # Check the dependencies of an imported function.
#' deps(reg1)
#' # Check the dependencies of commands in the workflow plan.
#' deps(my_plan$command[1])
#' deps(my_plan$command[4])
#' # Plot the interactive network visualization of the workflow.
#' config <- drake_config(my_plan)
#' vis_drake_graph(config)
#' # Run the workflow to build all the targets in the plan.
#' make(my_plan)
#' # For the reg2() model on the small dataset,
#' # the p-value is so small that there may be an association
#' # between weight and fuel efficiency after all.
#' readd(coef_regression2_small)
#' # Remove the whole cache.
#' clean(destroy = TRUE)
#' # Clean up the imported file.
#' unlink('report.Rmd')
#' })
#' }
load_basic_example <- function(
  envir = parent.frame(),
  cache = NULL,
  report_file = "report.Rmd",
  overwrite = FALSE,
  to = report_file,
  verbose = TRUE,
  force = FALSE
){
  if (to != report_file){
    warning(
      "In load_basic_example(), argument 'to' is deprecated. ",
      "Use 'report_file' instead."
    )
  }

  eval(parse(text = "base::require(drake, quietly = TRUE)"))
  eval(parse(text = "base::require(knitr, quietly = TRUE)"))
  mtcars <- get("mtcars")

  # Bootstrapped datasets from mtcars.
  envir$simulate <- function(n){
    # Pick a random set of cars to bootstrap from the mtcars data.
    index <- sample.int(n = nrow(mtcars), size = n, replace = TRUE)
    data <- mtcars[index, ]

    # x is the car's weight, and y is the fuel efficiency.
    data.frame(
      x = data$wt,
      y = data$mpg
    )
  }

  # Is there a linear relationship between weight and fuel efficiency?
  envir$reg1 <- function(d) {
    lm(y ~ +x, data = d)
  }

  # Is there a QUADRATIC relationship between weight and fuel efficiency?
  envir$reg2 <- function(d) {
    d$x2 <- d$x ^ 2
    lm(y ~ x2, data = d)
  }

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

  # External file targets and dependencies should be
  # single-quoted.  Use double quotes to remove any special
  # meaning from character strings.  Single quotes inside
  # imported functions are ignored, so this mechanism only
  # works inside the drake_plan my_plan data frame.  WARNING:
  # drake cannot track entire directories (folders).
  report <- drake_plan(report.md = knit("report.Rmd", quiet = TRUE),
    file_targets = TRUE, strings_in_dots = "filenames")

  # Row order doesn't matter in the drake_plan my_plan.
  envir$my_plan <- rbind(report, datasets,
    analyses, results)

  # Write the R Markdown source for a dynamic knitr report
  report <- system.file(
    file.path("examples", "basic", "report.Rmd"),
    package = "drake",
    mustWork = TRUE
  )
  if (file.exists(report_file) & overwrite){
    warning("Overwriting file 'report.Rmd'.")
  }
  file.copy(from = report, to = report_file, overwrite = overwrite)
  invisible(drake_config(
    plan = envir$my_plan,
    envir = envir,
    cache = cache,
    force = force,
    verbose = verbose
  ))
}
