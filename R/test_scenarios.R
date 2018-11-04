default_testing_scenario <- "local_parLapply_1"
test_option_name <- "drake_test_scenario"

testing_scenarios <- function() {
  file <- file.path("testing", "scenarios.csv")
  path <- system.file(file, package = "drake", mustWork = TRUE)
  x <- read.csv(path, stringsAsFactors = FALSE)
  rn <- paste(
    x$envir,
    x$parallelism,
    x$jobs,
    x$backend,
    x$caching,
    sep = "_"
  )
  rn <- gsub(pattern = "_*$", replacement = "", x = rn)
  rownames(x) <- rn
  x$backend <- backend_code(x$backend)
  x$envir <- envir_code(x$envir)
  x$caching[!nzchar(x$caching)] <- "worker"
  apply_skip_os(x)
}

backend_code <- function(x) {
  ifelse(
    nzchar(x),
    paste0("future::plan(", x, ")"),
    x
  )
}

envir_code <- function(x) {
  ifelse(
    x == "local",
    "new.env(parent = globalenv())",
    "globalenv()"
  )
}

# For the table of possible testing scenarios x.
apply_skip_os <- function(x) {
  x$skip_os <- ""
  skip_on_windows <- grepl("mclapply|clustermq", x$parallelism)
  x$skip_os[skip_on_windows] <- "windows"
  x
}

testing_scenario_names <- function() {
  rownames(testing_scenarios())
}

get_testing_scenario <- function() {
  scenario <- get_testing_scenario_name()
  testing_scenarios()[scenario, ]
}

get_testing_scenario_name <- function() {
  scenario <- getOption(test_option_name)
  if (!length(scenario)) {
    scenario <- match.arg(
      arg = default_testing_scenario,
      choices = testing_scenario_names()
    )
  }
  scenario
}

set_testing_scenario <- function(scenario = NULL) {
  scenario <- match.arg(scenario, choices = testing_scenario_names())
  new <- list()
  new[[test_option_name]] <- scenario
  options(new)
}

should_skip <- function(scenario_name, os = this_os()) {
  scenarios <- testing_scenarios()
  scenario_name <- match.arg(
    arg = scenario_name,
    choices = testing_scenario_names()
  )
  os %in% scenarios[scenario_name, ]$skip_os
}

test_scenarios <- function(
  scenario_names = testing_scenario_names(),
  unit_test_dir = unit_test_files(),
  skip_criterion = should_skip,
  ...
) {
  assert_pkg("testthat")
  scenarios <- testing_scenarios()
  for (index in seq_along(along.with = scenario_names)) {
    scenario_names[index] <- match.arg(
      arg = scenario_names[index],
      choices = testing_scenario_names()
    )
  }
  for (scenario_name in scenario_names) {
    skip <- skip_criterion(scenario_name)
    msg <- ifelse(skip, "skip", "run")
    message(scenario_name, ": ", msg, sep = "")
    new <- list()
    new[[test_option_name]] <- scenario_name
    if (!skip) {
      with_options(
        new = new,
        testthat::test_dir(
          path = unit_test_dir,
          reporter = "summary"
        )
      )
    }
  }
}
