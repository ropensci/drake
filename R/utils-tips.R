# Output a random tip about drake.
drake_tip_ <- function() {
  tips <- c(
    "map_plan() sets up iterative function calls on a grid of arguments
     to create a drake plan.",

    "Use diagnose() to retrieve diagnostic metadata:
     errors, warnings, messages, commands, runtimes, etc.",

    "Use drake_example(\"main\") to generate the code files
     for a friendly example workflow.",

    "Check out the reference website (https://ropensci.github.io/drake/)
     and user manual (https://ropenscilabs.github.io/drake-manual/).",

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
