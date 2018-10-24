#' @title Download and save the code and data files of an example
#'   `drake`-powered project.
#' @description The `drake_example()` function downloads a
#'   folder from <https://github.com/wlandau/drake-examples>.
#'   (Really, it downloads one of the zip files listed at
#'   <https://github.com/wlandau/drake-examples/tree/gh-pages>
#'   and unzips it. Do not include the `.zip` extension
#'   in the `example` argument.)
#' @seealso [drake_examples()], [make()]
#' @export
#' @return `NULL`
#' @param example name of the example.
#'   The possible values are the names of the folders at
#'   <https://github.com/wlandau/drake-examples>.
#' @param to Character scalar,
#'   the folder containing the code files for the example.
#'   passed to the `exdir` argument of `utils::unzip()`.
#' @param destination Deprecated, use `to` instead.
#' @param overwrite Logical, whether to overwrite an existing folder
#'   with the same name as the drake example.
#' @param quiet logical, passed to `downloader::download()`
#'   and thus `utils::download.file()`. Whether
#'   to download quietly or print progress.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' drake_examples() # List all the drake examples.
#' # Sets up the same example as https://ropenscilabs.github.io/drake-manual/mtcars.html # nolint
#' drake_example("mtcars")
#' # Sets up the SLURM example.
#' drake_example("slurm")
#' })
#' }
drake_example <- function(
  example = "main",
  to = getwd(),
  destination = NULL,
  overwrite = FALSE,
  quiet = TRUE
){
  assert_pkg("downloader")
  if (!is.null(destination)){
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
#' @description You can find the code files of the examples at
#'   <https://github.com/wlandau/drake-examples>.
#'   The `drake_examples()` function downloads the list of examples
#'   from <https://wlandau.github.io/drake-examples/examples.md>,
#'   so you need an internet connection.
#' @export
#' @seealso [drake_example()], [make()]
#' @return Names of all the drake examples.
#' @param quiet logical, passed to `downloader::download()`
#'   and thus `utils::download.file()`. Whether
#'   to download quietly or print progress.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' drake_examples() # List all the drake examples.
#' # Sets up the example from
#' # https://ropenscilabs.github.io/drake-manual/mtcars.html
#' drake_example("mtcars")
#' # Sets up the SLURM example.
#' drake_example("slurm")
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
  scan(destfile, what = character(1), quiet = TRUE) %>%
    fs::path_ext_remove() %>%
    sort()
}

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
#' @seealso [clean_mtcars_example()]
#' @return nothing
#' @inheritParams drake_config
#' @param envir The environment to load the example into.
#'   Defaults to your workspace.
#'   For an insulated workspace,
#'   set `envir = new.env(parent = globalenv())`.
#' @param report_file where to write the report file. Deprecated.
#'   In a future release, the report file will always be
#'   `report.Rmd` and will always be written to your
#'   working directory (current default).
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
#' # Clean up the example.
#' clean_mtcars_example()
#' })
#' }
load_mtcars_example <- function(
  envir = parent.frame(),
  report_file = NULL,
  overwrite = FALSE,
  force = FALSE
){
  if (!is.null(report_file)){
    warning(
      "The `report_file` argument of load_mtcars_example() ",
      "is deprecated and will be removed in a future release.",
      call. = FALSE
    )
  }
  if (is.null(report_file)){
    report_file <- "report.Rmd"
  }
  populate_mtcars_example_envir(envir = envir)
  if (file.exists(report_file) && overwrite){
    warning("Overwriting file ", report_file, call. = FALSE)
  }
  report_path <- system.file(
    file.path("examples", "mtcars", "report.Rmd"),
    package = "drake",
    mustWork = TRUE
  )
  file.copy(from = report_path, to = report_file, overwrite = overwrite)
  invisible()
}

populate_mtcars_example_envir <- function(envir){
  eval(parse(text = "suppressPackageStartupMessages(require(drake))"))
  eval(parse(text = "suppressPackageStartupMessages(require(knitr))"))
  mtcars <- NULL
  local(envir = envir, {
    random_rows <- function(data, n){
      data[sample.int(n = nrow(data), size = n, replace = TRUE), ]
    }
    simulate <- function(n){
      data <- random_rows(data = mtcars, n = n)
      data.frame(
        x = data$wt,
        y = data$mpg
      )
    }
    reg1 <- function(d){
      lm(y ~ + x, data = d)
    }
    reg2 <- function(d){
      d$x2 <- d$x ^ 2
      lm(y ~ x2, data = d)
    }
  })
  envir$my_plan <- mtcars_plan()
  invisible()
}

mtcars_plan <- function(){
  simulate <- reg1 <- dataset__ <- reg2 <- analysis__ <- knit <- NULL
  mtcars <- NULL
  my_datasets <- drake_plan(
    small = simulate(48),
    large = simulate(64)
  )
  methods <- drake_plan(
    regression1 = reg1(dataset__),
    regression2 = reg2(dataset__)
  )
  my_analyses <- evaluate_plan(
    methods, wildcard = "dataset__",
    values = my_datasets$target
  )
  summary_types <- drake_plan(
    summ = suppressWarnings(summary(analysis__$residuals)),
    coef = suppressWarnings(summary(analysis__))$coefficients
  )
  my_summaries <- evaluate_plan(
    summary_types,
    wildcard = "analysis__",
    values = my_analyses$target
  )
  report <- drake_plan(
    report = knit(knitr_in("report.Rmd"), file_out("report.md"), quiet = TRUE),
    strings_in_dots = "literals"
  )
  bind_plans(report, my_datasets, my_analyses, my_summaries)
}

#' @title Clean the mtcars example from `drake_example("mtcars")`
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
#' # Clean up the example.
#' clean_mtcars_example()
#' })
#' }
clean_mtcars_example <- function(){
  clean(destroy = TRUE, search = FALSE)
  unlink("report.Rmd")
  invisible()
}

#' @title Load the main example from `drake_example("main")`
#' @description The main example itself lives at
#' <https://github.com/wlandau/drake-examples/tree/master/main>. # nolint
#' Use `drake_example("main")` to download the code.
#' The chapter of the user manual at
#' <https://ropenscilabs.github.io/drake-manual/main.html> # nolint
#' also walks through the main example.
#' This function also writes/overwrites
#' the files `report.Rmd` and `raw_data.xlsx`.
#' @export
#' @seealso [clean_main_example()]
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
#' if (requireNamespace("downloader")){
#' # Populate your workspace and write 'report.Rmd' and 'raw_data.xlsx'.
#' load_main_example() # Get the code: drake_example("main")
#' # Run the project with make(plan).
#' # Make sure you have these packages installed first:
#' # dplyr, forcats, ggplot2, readxl, and rmarkdown
#' make(plan) # Build the targets.
#' readd(hist) # Show the ggplot2 histogram.
#' # Clean up the example.
#' clean_main_example()
#' }
#' })
#' }
load_main_example <- function(
  envir = parent.frame(),
  report_file = "report.Rmd",
  overwrite = FALSE,
  force = FALSE
){
  dir <- tempfile()
  drake_example(example = "main", to = dir)
  source(file.path(dir, "main", "R", "setup.R"), local = envir)
  envir$plan <- source(
    file.path(dir, "main", "R", "plan.R"),
    local = TRUE
  )$value
  for (file in c("report.Rmd", "raw_data.xlsx")){
    if (file.exists(file) & overwrite){
      warning("Overwriting file ", file, call. = FALSE)
    }
    file.copy(
      from = file.path(dir, "main", file),
      to = file,
      overwrite = overwrite
    )
  }
  invisible()
}

#' @title Clean the main example from `drake_example("main")`
#' @description This function deletes files. Use at your own risk.
#'   Destroys the `.drake/` cache and the `report.Rmd` file
#'   in the current working directory. Your working directory
#'   (`getcwd()`) must be the folder from which you first ran
#'   `load_main_example()` and `make(my_plan)`.
#' @export
#' @return Nothing.
#' @seealso [load_main_example()], [clean()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (requireNamespace("downloader")){
#' # Populate your workspace and write 'report.Rmd' and 'raw_data.xlsx'.
#' load_main_example() # Get the code: drake_example("main")
#' make(plan)
#' readd(hist) # Show the ggplot2 histogram.
#' # Clean up the example.
#' clean_main_example()
#' }
#' })
#' }
clean_main_example <- function(){
  clean(destroy = TRUE, search = FALSE)
  unlink(c("report.Rmd", "raw_data.xlsx"))
  invisible()
}
