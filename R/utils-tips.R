# Output a random tip about drake.
drake_tip_ <- function() {
  tips <- c(
    "A new and improved way to create large drake plans:
     https://ropenscilabs.github.io/drake-manual/plans.html#large-plans",

    "Use diagnose() to retrieve
     errors, warnings, messages, commands, runtimes, etc.",

    "Use drake_example() to download code for a small drake workflow.",

    "Check out the reference website https://ropensci.github.io/drake
     and user manual https://ropenscilabs.github.io/drake-manual.",

    "drake quickstart:
     load_mtcars_example();
     make(my_plan);
     readd(small)"
  )
  tips <- wrap_text(tips)
  sample(tips, 1)
}

drake_tip_message <- function() {
  packageStartupMessage(drake_tip_())
}

wrap_text <- Vectorize(
  function(x) {
    x <- paste(strwrap(x), collapse = "\n")
    unname(x)
  },
  "x"
)
