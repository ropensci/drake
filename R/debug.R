dbug = function(envir = parent.frame()){
  force(envir)
  eval(parse(text = "f <- function(x) {g(x) + a}"), envir = envir)
  eval(parse(text = "g <- function(y) {h(y) + b}"), envir = envir)
  eval(parse(text = "h <- function(y) {i(y) + j(y)}"), envir = envir)
  eval(parse(text = "i <- function(x) {x+1}"), envir = envir)
  eval(parse(text = "j <- function(x) {x+2 + d}"), envir = envir)
  eval(parse(text = "a <- 15"), envir = envir)
  eval(parse(text = "b <- 20"), envir = envir)
  eval(parse(text = "d <- 25"), envir = envir)
  saveRDS(1:10, "input.rds")
  out = plan(
    "'intermediatefile.rds'" = "saveRDS(combined, \"intermediatefile.rds\")",
    yourinput = "f(1+1)",
    nextone = "myinput + g(7)",
    combined = "nextone + yourinput",
    myinput = "readRDS('input.rds')",
    final = "readRDS('intermediatefile.rds')"
  )
  out
}

dclean = function(envir = parent.frame()){
  force(envir)
  rm(list = c("a", "b", "d", "f", "g", "h", "i", "j"), envir = envir)
  unlink("input.rds")
  unlink("intermediatefile.rds")
  unlink(".drake", recursive = TRUE)
}
