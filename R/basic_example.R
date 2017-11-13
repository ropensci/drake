#' @title Function \code{load_basic_example}
#' @description Loads the basic example into your workspace
#' (or the environment you specify).
#' Also writes/overwrites the file \code{report.Rmd}.
#' For a thorough walkthrough of how to set up this example, see the
#' quickstart vignette: \code{vignette('quickstart')}. Alternatively,
#' call \code{\link{example_drake}('basic')} to generate an R script
#' that builds up this example step by step.
#' @export
#' @return The workflow plan data frame of the basic example.
#' @param envir The environment to load the example into.
#' Defaults to your workspace.
#' For an insulated workspace,
#' set \code{envir = new.env(parent = globalenv())}.
#' @param report_file where to write the report file \code{report.Rmd}.
#' @param to deprecated, where to write the dynamic report source file
#' \code{report.Rmd}
#' @param overwrite logical, whether to overwrite an
#' existing file \code{report.Rmd}
#' @examples
#' \dontrun{
#' # Populate your workspace and write 'report.Rmd'.
#' load_basic_example()
#' # Check the dependencies of an imported function.
#' deps(reg1)
#' # Check the dependencies of commands in the workflow plan.
#' deps(my_plan$command[1])
#' deps(my_plan$command[4])
#' # Plot the interactive network visualization of the workflow.
#' vis_drake_graph(my_plan)
#' # Run the workflow to build all the targets in the plan.
#' make(my_plan)
#' # Remove the whole cache.
#' clean(destroy = TRUE)
#' # Clean up the imported file.
#' unlink('report.Rmd')
#' }
load_basic_example <- function(
  envir = parent.frame(), report_file = "report.Rmd", overwrite = FALSE,
  to = report_file
){
  if (to != report_file){
    warning(
      "In load_basic_example(), argument 'to' is deprecated. ",
      "Use 'report_file' instead."
    )
  }

  eval(parse(text = "base::require(drake, quietly = TRUE)"))
  eval(parse(text = "base::require(knitr, quietly = TRUE)"))

  # User-defined functions
  envir$simulate <- function(n) {
    data.frame(x = stats::rnorm(n), y = rpois(n, 1))
  }

  envir$reg1 <- function(d) {
    lm(y ~ +x, data = d)
  }

  envir$reg2 <- function(d) {
    d$x2 <- d$x ^ 2
    lm(y ~ x2, data = d)
  }

  # construct workflow plan

  # remove 'undefinded globals' errors in R CMD check
  large <- small <-
    simulate <- knit <- my_knit <- report_dependencies <-
    reg1 <- reg2 <- coef_regression2_small <- NULL

  datasets <- workplan(small = simulate(5), large = simulate(50))

  methods <- workplan(list = c(
    regression1 = "reg1(dataset__)",
    regression2 = "reg2(dataset__)"))

  # Same as evaluate_plan(methods, wildcard = 'dataset__',
  #   values = datasets$output).
  analyses <- plan_analyses(methods, datasets = datasets)

  summary_types <- workplan(list = c(
    summ = "suppressWarnings(summary(analysis__))",
    coef = "coefficients(analysis__)"))

  # plan_summaries() also uses evaluate_plan(): once with expand = TRUE,
  # once with expand = FALSE
  # skip 'gather' (workplan my_plan is more readable)
  results <- plan_summaries(summary_types, analyses, datasets, gather = NULL)

  # External file targets and dependencies should be
  # single-quoted.  Use double quotes to remove any special
  # meaning from character strings.  Single quotes inside
  # imported functions are ignored, so this mechanism only
  # works inside the workplan my_plan data frame.  WARNING:
  # drake cannot track entire directories (folders).
  report <- workplan(report.md = knit("report.Rmd", quiet = TRUE),
    file_targets = TRUE, strings_in_dots = "filenames")

  # Row order doesn't matter in the workplan my_plan.
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
  invisible(envir$my_plan)
}
