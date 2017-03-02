dbug = function(clean = TRUE){
  if(clean) dclean()
  envir = new.env(parent = globalenv())
  eval(parse(text = "f <- function(x) {g(x) + a}"), envir = envir)
  eval(parse(text = "g <- function(y) {h(y) + b}"), envir = envir)
  eval(parse(text = "h <- function(y) {i(y) + j(y)}"), envir = envir)
  eval(parse(text = "i <- function(x) {x+1}"), envir = envir)
  eval(parse(text = "j <- function(x) {x+2 + c}"), envir = envir)
  eval(parse(text = "a <- 15"), envir = envir)
  eval(parse(text = "b <- 20"), envir = envir)
  eval(parse(text = "c <- 25"), envir = envir)

  saveRDS(1:10, "input.rds")
  # set.seed(0); saveRDS(rnorm(100000), "input.rds") # test rehashing

  plan = plan(list = c(
    "'intermediatefile.rds'" = 
      "saveRDS(combined, \"intermediatefile.rds\")",
    yourinput = "f(1+1)",
    nextone = "myinput + g(7)",
    combined = "nextone + yourinput",
    myinput = "readRDS('input.rds')",
    final = "readRDS('intermediatefile.rds')"
  ))
  args = setup(plan, targets = plan$target, envir = envir, jobs = 1,
    verbose = TRUE, prework = character(0), command = "make", 
    args = character(0))
  args
}

dclean = function(){
  unlink(".drake", recursive = TRUE)
  unlink("input.rds")
  unlink("intermediatefile.rds")
  unlink("Makefile")
}

