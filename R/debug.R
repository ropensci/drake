dbug <- function(clean = TRUE){
  if (clean){
    dclean()
  }
  envir <- new.env(parent = globalenv()) # for shipped unit tests
  # Note that `envir = globalenv()` needs to work here too.
  # This is addressed in issue #58

  imports <- c(
    "f <- function(x) {g(x) + a}",
    "g <- function(y) {h(y) + b}",
    "h <- function(y) {i(y) + j(y)}",
    "i <- function(x) {x+1}",
    "j <- function(x) {x+2 + c}",
    "a <- 15",
    "b <- 20",
    "c <- 25"
    )
  for (import in imports){
    eval(parse(text = import), envir = envir)
  }

  saveRDS(1:10, "input.rds") # small files are always rehashed
  # TODO: implement as a test on non-CRAN systems:
  # relates to issues #58, #53, and #4 (closed, but relevant discussion)
  # example code: set.seed(0); saveRDS(rnorm(100000), "input.rds")

  plan <- plan(list = c(
      "'intermediatefile.rds'" =
        "saveRDS(combined, \"intermediatefile.rds\")",
      yourinput = "f(1+1)",
      nextone = "myinput + g(7)",
      combined = "nextone + yourinput",
      myinput = "readRDS('input.rds')",
      final = "readRDS('intermediatefile.rds')"
      ))
  config <- build_config(
    plan = plan,
    targets = plan$target,
    envir = envir,
    parallelism = "mclapply",
    jobs = 1,
    prepend = character(0),
    verbose = FALSE,
    packages = character(0),
    prework = character(0),
    command = "make",
    args = character(0)
    )
  config
}

dclean = function(){
  unlink(c(".drake", "intermediatefile.rds", "input.rds", 
    "Makefile", "report.md", "report.Rmd"), recursive = TRUE)
}
