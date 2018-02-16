dbug <- function() {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  envir <- dbug_envir(envir)
  dbug_files()
  plan <- dbug_plan()
  drake_config(plan = plan, targets = plan$target,
    envir = envir, parallelism = scenario$parallelism,
    jobs = scenario$jobs, verbose = FALSE,
    session_info = FALSE,
    log_progress = TRUE
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
  drake_plan(
    saveRDS(combined, file_output(intermediatefile.rds)),
    yourinput = f(1+1),
    nextone = myinput + g(7),
    combined = nextone + yourinput,
    myinput = readRDS(file_input(input.rds)),
    final = readRDS(file_input(intermediatefile.rds)))
}

dbug_files <- function() {
  saveRDS(1:10, "input.rds")
}
