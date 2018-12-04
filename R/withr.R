# This file contains a short collection of functions
# borrowed from the `withr` package:
# https://github.com/r-lib/withr.
# Copyright RStudio, Inc.
# GPL (>=2)

with_dir <- function (new, code) {
  old <- alt_setwd(new)
  on.exit(alt_setwd(old))
  force(code)
}

# goodpractice::gp() complains about
# legitimate uses of setwd().
alt_setwd <- function(dir) {
  text <- paste0("setwd(\"", dir, "\")")
  eval(parse(text = text))
}

with_options <- function (new, code) {
  old <- set_options(new_options = new)
  on.exit(set_options(new_options = old))
  force(code)
}

set_options <- function(new_options) {
  do.call(options, as.list(new_options))
}

with_seed <- function(seed, code) {
  force(seed)
  with_preserve_seed({
    set.seed(seed)
    code
  })
}

with_preserve_seed <- function(code) {
  old_seed <- get_valid_seed()
  on.exit(assign(".Random.seed", old_seed, globalenv()), add = TRUE)
  code
}

get_valid_seed <- function() {
  seed <- get_seed()
  if (is.null(seed)) {
    # Trigger initialisation of RNG
    sample.int(1L) # nocov
    seed <- get_seed() # nocov
  }
  seed
}

get_seed <- function() {
  no_seed_yet <- !exists(
    ".Random.seed",
    globalenv(),
    mode = "integer",
    inherits = FALSE
  )
  if (no_seed_yet) {
    return(NULL) # nocov
  }
  get(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
}
