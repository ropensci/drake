dbug = function(){
  dclean()
  envir = new.env(parent = globalenv())
  eval(parse(text = "f <- function(x) {g(x) + a}"), envir = envir)
  eval(parse(text = "g <- function(y) {h(y) + b}"), envir = envir)
  eval(parse(text = "h <- function(y) {i(y) + j(y)}"), envir = envir)
  eval(parse(text = "i <- function(x) {x+1}"), envir = envir)
  eval(parse(text = "j <- function(x) {x+2 + d}"), envir = envir)
  eval(parse(text = "a <- 15"), envir = envir)
  eval(parse(text = "b <- 20"), envir = envir)
  eval(parse(text = "d <- 25"), envir = envir)
  saveRDS(1:10, "input.rds")
  plan = plan(list = c(
    "'intermediatefile.rds'" = "saveRDS(combined, \"intermediatefile.rds\")",
    yourinput = "f(1+1)",
    nextone = "myinput + g(7)",
    combined = "nextone + yourinput",
    myinput = "readRDS('input.rds')",
    final = "readRDS('intermediatefile.rds')"
  ))
  args = arglist(plan, targets = plan$target, envir = envir, jobs = 1) 
  args$graph = build_graph(plan = args$plan, targets = args$targets, envir = args$envir)
  args
}

dclean = function(){
  unlink("input.rds")
  unlink("intermediatefile.rds")
  unlink(".drake", recursive = TRUE)
}
