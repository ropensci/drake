#' @title Output a random tip about drake.
#' @description Tips are usually related to news and usage.
#' @export
#' @return A character scalar with a tip on how to use drake.
#' @examples
#' drake_tip() # Show a tip about using drake.
#' message(drake_tip()) # Print out a tip as a message.
drake_tip <- function() {
  tips <- c(
    "Check out the new file API: file_in(), file_out(), knitr_in().
     Embrace the new API in your R session by calling
     pkgconfig::set_config(\"drake::strings_in_dots\" = \"literals\")",

    "Use diagnose() to retrieve diagnostic metadata:
     errors, warnings, messages, commands, runtimes, etc.",

    "Use drake_example(\"main\") to generate the code files
     for a friendly example workflow.",

    "Check out the documentation website: https://ropensci.github.io/drake/",

    "Use suppressPackageStartupMessages() to eliminate
     package startup messages like this one.",

    "Drake quickstart:
     load_mtcars_example();
     vis_drake_graph(my_plan);
     make(my_plan);
     vis_drake_graph(my_plan)"
  ) %>% wrap_text
  sample(tips, 1)
}

drake_tip_message <- function() {
  packageStartupMessage(drake_tip())
}

wrap_text <- Vectorize(function(x) {
  paste(strwrap(x), collapse = "\n") %>% unname
},
"x")
