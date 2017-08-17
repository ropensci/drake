dbug = function(clean = TRUE){
  if(clean) dclean()

  opt <- test_opt()
  envir <- eval(parse(text = opt$envir))
  envir <- dbug_envir(envir)
  dbug_files()
  plan <- dbug_plan()

  config <- build_config(
    plan = plan, 
    targets = plan$target, 
    envir = envir,
    parallelism = opt$parallelism, 
    jobs = opt$jobs, 
    prepend = character(0),
    verbose = FALSE, 
    packages = character(0), 
    prework = character(0),
    command = "make", 
    args = character(0))

#  show_config_opts(config)
  config
}

dclean = function(){
  unlink(c(".drake", "intermediatefile.rds", "input.rds",
    "Makefile", "report.md", "report.Rmd"), recursive = TRUE)
}

dbug_envir <- function(envir){
  force(envir)
  imports <- c(
    "f <- function(x) {g(x) + a}",
    "g <- function(y) {h(y) + b}",
    "h <- function(y) {i(y) + j(y)}",
    "i <- function(x) {x+1}",
    "j <- function(x) {x+2 + c}",
    "a <- 15",
    "b <- 20",
    "c <- 25")
  for(import in imports)
    eval(parse(text = import), envir = envir)
  envir
}

dbug_plan <- function(){
  plan(list = c(
    "'intermediatefile.rds'" = 
      "saveRDS(combined, \"intermediatefile.rds\")",
    yourinput = "f(1+1)",
    nextone = "myinput + g(7)",
    combined = "nextone + yourinput",
    myinput = "readRDS('input.rds')",
    final = "readRDS('intermediatefile.rds')"
  ))
}

dbug_files <- function(){
  saveRDS(1:10, "input.rds")
}
