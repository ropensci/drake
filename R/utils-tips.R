#' @title Output a random tip about drake.
#' @description Tips are usually related to news and usage.
#' @export
#' @return A character scalar with a tip on how to use drake.
#' @examples
#' drake_tip() # Show a tip about using drake.
#' message(drake_tip()) # Print out a tip as a message.
drake_tip <- function() {
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
  packageStartupMessage(drake_tip())
}

wrap_text <- Vectorize(
  function(x) {
    x <- paste(strwrap(x), collapse = "\n")
    unname(x)
  },
  "x"
)
