#' @title Download the files of an example `drake` project.
#' `r lifecycle::badge("stable")`
#' @description The `drake_example()` function downloads a
#'   folder from `https://github.com/wlandau/drake-examples`.
#'   By default, it creates a new folder with the example name
#'   in your current working directory. After the files are written,
#'   have a look at the enclosed `README` file.
#'   Other instructions are available in the files at
#'   `https://github.com/wlandau/drake-examples`.
#' @seealso [drake_examples()], [make()]
#' @export
#' @return `NULL`
#' @param example Name of the example.
#'   The possible values are the names of the folders at
#'   `https://github.com/wlandau/drake-examples`.
#' @param to Character scalar,
#'   the folder containing the code files for the example.
#'   passed to the `exdir` argument of `utils::unzip()`.
#' @param destination Deprecated; use `to` instead.
#' @param overwrite Logical, whether to overwrite an existing folder
#'   with the same name as the drake example.
#' @param quiet Logical, passed to `downloader::download()`
#'   and thus `utils::download.file()`. Whether
#'   to download quietly or print progress.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (requireNamespace("downloader")) {
#' drake_examples() # List all the drake examples.
#' # Sets up the same example from load_mtcars_example()
#' drake_example("mtcars")
#' # Sets up the SLURM example.
#' drake_example("slurm")
#' }
#' })
#' }
drake_example <- function(
  example = "main",
  to = getwd(),
  destination = NULL,
  overwrite = FALSE,
  quiet = TRUE
) {
  assert_pkg("downloader")
  if (!is.null(destination)) {
    warning(
      "The 'destination' argument of drake_example() is deprecated. ",
      "Use 'to' instead."
    )
    to <- destination
  }
  url <- file.path(
    "https://wlandau.github.io/drake-examples",
    paste0(example, ".zip")
  )
  zip <- paste0(tempfile(), ".zip")
  downloader::download(url = url, destfile = zip, quiet = quiet)
  utils::unzip(zip, exdir = to, overwrite = overwrite)
  invisible()
}

#' @title List the names of all the drake examples.
#' `r lifecycle::badge("stable")`
#' @description You can find the code files of the examples at
#'   `https://github.com/wlandau/drake-examples`.
#'   The `drake_examples()` function downloads the list of examples
#'   from `https://wlandau.github.io/drake-examples/examples.md`,
#'   so you need an internet connection.
#' @export
#' @seealso [drake_example()], [make()]
#' @return Names of all the drake examples.
#' @param quiet Logical, passed to `downloader::download()`
#'   and thus `utils::download.file()`. Whether
#'   to download quietly or print progress.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (requireNamespace("downloader")) {
#' drake_examples() # List all the drake examples.
#' # Sets up the example from load_mtcars_example()
#' drake_example("mtcars")
#' # Sets up the SLURM example.
#' drake_example("slurm")
#' }
#' })
#' }
drake_examples <- function(quiet = TRUE) {
  assert_pkg("downloader")
  destfile <- tempfile()
  downloader::download(
    url = "https://wlandau.github.io/drake-examples/examples.md",
    destfile = destfile,
    quiet = quiet
  )
  out <- scan(destfile, what = character(1), quiet = TRUE)
  out <- gsub(pattern = "\\.zip$", replacement = "", x = out)
  sort(out)
}

#' @title Load the mtcars example.
#' `r lifecycle::badge("stable")`
#' @description Is there an association between
#' the weight and the fuel efficiency of cars?
#' To find out, we use the mtcars example from `drake_example("mtcars")`.
#' The mtcars dataset itself only has 32 rows,
#' so we generate two larger bootstrapped datasets
#' and then analyze them with regression models.
#' Finally, we summarize the regression models
#' to see if there is an association.
#' @details Use `drake_example("mtcars")` to get the code
#' for the mtcars example.
#' This function also writes/overwrites
#' the file, `report.Rmd`.
#' @export
#' @seealso [clean_mtcars_example()] [drake_examples()]
#' @return Nothing.
#' @param envir The environment to load the example into.
#'   Defaults to your workspace.
#'   For an insulated workspace,
#'   set `envir = new.env(parent = globalenv())`.
#' @param report_file Where to write the report file. Deprecated.
#'   In a future release, the report file will always be
#'   `report.Rmd` and will always be written to your
#'   working directory (current default).
#' @param overwrite Logical, whether to overwrite an
#'   existing file `report.Rmd`.
#' @param force Deprecated.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' # Populate your workspace and write 'report.Rmd'.
#' load_mtcars_example() # Get the code: drake_example("mtcars")
#' # Check the dependencies of an imported function.
#' deps_code(reg1)
#' # Check the dependencies of commands in the workflow plan.
#' deps_code(my_plan$command[1])
#' deps_code(my_plan$command[4])
#' # Plot the interactive network visualization of the workflow.
#' outdated(my_plan) # Which targets are out of date?
#' # Run the workflow to build all the targets in the plan.
#' make(my_plan)
#' outdated(my_plan) # Everything should be up to date.
#' # For the reg2() model on the small dataset,
#' # the p-value is so small that there may be an association
#' # between weight and fuel efficiency after all.
#' readd(coef_regression2_small)
#' # Clean up the example.
#' clean_mtcars_example()
#' }
#' })
#' }
load_mtcars_example <- function(
  envir = parent.frame(),
  report_file = NULL,
  overwrite = FALSE,
  force = FALSE
) {
  force(envir)
  deprecate_force(force)
  if (!is.null(report_file)) {
    warn0(
      "The `report_file` argument of load_mtcars_example() ",
      "is deprecated and will be removed in a future release."
    )
  }
  if (is.null(report_file)) {
    report_file <- "report.Rmd"
  }
  populate_mtcars_example_envir(envir = envir)
  if (file.exists(report_file) && overwrite) {
    warn0("Overwriting file ", report_file)
  }
  report_path <- system.file(
    file.path("rmarkdown", "examples", "mtcars", "report.Rmd"),
    package = "drake",
    mustWork = TRUE
  )
  file.copy(from = report_path, to = report_file, overwrite = overwrite)
  invisible()
}

populate_mtcars_example_envir <- function(envir) {
  assert_pkg("datasets")
  force(envir)
  mtcars <- lm <- NULL
  local(envir = envir, {
    random_rows <- function(data, n) {
      data[sample.int(n = nrow(data), size = n, replace = TRUE), ]
    }
    simulate <- function(n) {
      data <- random_rows(data = datasets::mtcars, n = n)
      data.frame(
        x = data$wt,
        y = data$mpg
      )
    }
    reg1 <- function(d) {
      lm(y ~ + x, data = d)
    }
    reg2 <- function(d) {
      d$x2 <- d$x ^ 2
      lm(y ~ x2, data = d)
    }
  })
  envir$my_plan <- mtcars_plan()
  invisible()
}

mtcars_plan <- function() {
  report <- small <- large <- regression1_small <- regression1_large <-
    regression2_small <- regression2_large <- summ_regression1_small <-
    summ_regression1_large <- summ_regression2_small <-
    summ_regression2_large <- coef_regression1_small <-
    coef_regression1_large <- coef_regression2_small <-
    coef_regression2_large <- knit <- simulate <- reg1 <- reg2 <- NULL
  drake::drake_plan(
    report = knitr::knit(
      drake::knitr_in("report.Rmd"),
      drake::file_out("report.md"),
      quiet = TRUE
    ),
    small = simulate(48),
    large = simulate(64),
    regression1_small = reg1(small),
    regression1_large = reg1(large),
    regression2_small = reg2(small),
    regression2_large = reg2(large),
    summ_regression1_small =
      suppressWarnings(summary(regression1_small$residuals)),
    summ_regression1_large =
      suppressWarnings(summary(regression1_large$residuals)),
    summ_regression2_small =
      suppressWarnings(summary(regression2_small$residuals)),
    summ_regression2_large =
      suppressWarnings(summary(regression2_large$residuals)),
    coef_regression1_small =
      suppressWarnings(summary(regression1_small))$coefficients,
    coef_regression1_large =
      suppressWarnings(summary(regression1_large))$coefficients,
    coef_regression2_small =
      suppressWarnings(summary(regression2_small))$coefficients,
    coef_regression2_large =
      suppressWarnings(summary(regression2_large))$coefficients
  )
}

#' @title Clean the mtcars example from `drake_example("mtcars")`
#' `r lifecycle::badge("stable")`
#' @description This function deletes files. Use at your own risk.
#'   Destroys the `.drake/` cache and the `report.Rmd` file
#'   in the current working directory. Your working directory
#'   (`getcwd()`) must be the folder from which you first ran
#'   `load_mtcars_example()` and `make(my_plan)`.
#' @export
#' @return nothing
#' @seealso [load_mtcars_example()], [clean()]
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' # Populate your workspace and write 'report.Rmd'.
#' load_mtcars_example() # Get the code: drake_example("mtcars")
#' # Check the dependencies of an imported function.
#' deps_code(reg1)
#' # Check the dependencies of commands in the workflow plan.
#' deps_code(my_plan$command[1])
#' deps_code(my_plan$command[4])
#' # Plot the interactive network visualization of the workflow.
#' outdated(my_plan) # Which targets are out of date?
#' # Run the workflow to build all the targets in the plan.
#' make(my_plan)
#' outdated(my_plan) # Everything should be up to date.
#' # For the reg2() model on the small dataset,
#' # the p-value is so small that there may be an association
#' # between weight and fuel efficiency after all.
#' readd(coef_regression2_small)
#' # Clean up the example.
#' clean_mtcars_example()
#' }
#' })
#' }
clean_mtcars_example <- function() {
  clean(destroy = TRUE)
  unlink("report.Rmd")
  invisible()
}
