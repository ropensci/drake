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
  dir <- tempfile()
  drake_example(example = "mtcars", to = dir)
  source(file.path(dir, "mtcars", "R", "packages.R"), local = envir)
  source(file.path(dir, "mtcars", "R", "functions.R"), local = envir)
  envir$my_plan <- source(
    file.path(dir, "mtcars", "R", "plan.R"),
    local = TRUE
  )$value
  file.copy(
    from = file.path(dir, "mtcars", "report.Rmd"),
    to = "report.Rmd",
    overwrite = overwrite
  )
  invisible()
}
