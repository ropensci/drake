dbug <- function() {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  envir <- dbug_envir(envir)
  dbug_files()
  plan <- dbug_plan()

  build_config(plan = plan, targets = plan$target,
    envir = envir, parallelism = scenario$parallelism,
    jobs = scenario$jobs, prepend = character(0),
    verbose = FALSE, packages = character(0),
    prework = character(0), command = default_Makefile_command(),
    args = character(0),
    recipe_command = "Rscript -e",
    cache = NULL, clear_progress = TRUE
  )
}

dbug_envir <- function(envir) {
  force(envir)
  imports <- c(
    "f <- function(x) {g(x) + a}",
    "g <- function(y) {h(y) + b}",
    "h <- function(y) {i(y) + j(y)}",
    "i <- function(x) {x+1}",
    "j <- function(x) {x+2 + c}",
    "a <- 15", "b <- 20", "c <- 25")
  for (import in imports) eval(parse(text = import), envir = envir)
  envir
}

dbug_plan <- function() {
  plan(list = c(
    `'intermediatefile.rds'` = "saveRDS(combined, \"intermediatefile.rds\")",
    yourinput = "f(1+1)",
    nextone = "myinput + g(7)",
    combined = "nextone + yourinput",
    myinput = "readRDS('input.rds')",
    final = "readRDS('intermediatefile.rds')"))
}

dbug_files <- function() {
  saveRDS(1:10, "input.rds")
}
