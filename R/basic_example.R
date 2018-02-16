#' @title Load the basic example from `drake_example("basic")`
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
#' heavily-commented walkthrough. The basic example vignette at
#' <https://github.com/ropensci/drake/blob/master/vignettes/example-basic.Rmd> # nolint
#' and <https://ropensci.github.io/drake/articles/example-basic.html>
#' also walks through the basic example.
#' This function also writes/overwrites
#' the file, `report.Rmd`.
#' @export
#' @return A [drake_config()] configuration list.
#' @param envir The environment to load the example into.
#'   Defaults to your workspace.
#'   For an insulated workspace,
#'   set `envir = new.env(parent = globalenv())`.
#' @param seed integer, the root pseudo-random seed to use for your project.
#'   To ensure reproducibility across different R sessions,
#'   `set.seed()` and `.Random.seed` are ignored and have no affect on
#'   `drake` workflows. Conversely, [make()] does not change `.Random.seed`,
#'   even when pseudo-random numbers are generated.
#'
#'   On the first call to [make()] or [drake_config()], `drake`
#'   uses the random number generator seed from the `seed` argument.
#'   Here, if the `seed` is `NULL` (default), `drake` uses a `seed` of `0`.
#'   On subsequent [make()]s for existing projects, the project's
#'   cached seed will be used in order to ensure reproducibility.
#'   Thus, the `seed` argument must either be `NULL` or the same
#'   seed from the project's cache (usually the `.drake/` folder).
#'   To reset the random number generator seed for a project,
#'   use `clean(destroy = TRUE)`.
#' @param cache Optional `storr` cache to use.
#' @param report_file where to write the report file `report.Rmd`.
#' @param to deprecated, where to write the dynamic report source file
#'   `report.Rmd`
#' @param overwrite logical, whether to overwrite an
#'   existing file `report.Rmd`
#' @param verbose logical, whether to print console messages.
#' @param force logical, whether to force the loading of a
#'   non-back-compatible cache from a previous version of drake.
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
  seed = NULL,
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

  # External file targets and dependencies should be
  # single-quoted.  Use double quotes to remove any special
  # meaning from character strings.  Single quotes inside
  # imported functions are ignored, so this mechanism only
  # works inside the drake_plan my_plan data frame.  WARNING:
  # drake cannot track entire directories (folders).
  report <- drake_plan(
    file_path = knit(
      file_input(report.Rmd),
      file_output(report.md),
      quiet = TRUE
    )
  )

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
    seed = seed,
    cache = cache,
    force = force,
    verbose = verbose
  ))
}
