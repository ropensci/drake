#' @title Function drake_tip
#' @description Output a random tip about drake.
#' Tips are usually related to news and usage.
#' @export
#' @examples
#' drake_tip()
#' cat(drake_tip())
drake_tip <- function() {
  tips <- c(
    "Predict the runtime of the next make() with predict_runtime().
     See also build_times() and rate_limiting_times().",

    "Use example_drake('basic') to generate code files
     for a minimal drake example walkthrough.",

    "See the vignettes at
     https://cran.r-project.org/package=drake/vignettes for
     in-depth tutorials and other thorough documentation.",

    "Use suppressPackageStartupMessages() to eliminate
     package startup messages like this one.",

    "Check out the \"future_lapply\" backends. Example:
    load_basic_example();
    backend(future::multisession);
    make(my_plan, parallelism = \"future_lapply\")",

    "Drake quickstart:
     load_basic_example();
     plot_graph(my_plan);
     make(my_plan);
     plot_graph(my_plan)"
  ) %>% wrap_text
  sample(tips, 1)
}

drake_message <- function() {
  packageStartupMessage(drake_tip())
}

wrap_text <- Vectorize(function(x) {
  paste(strwrap(x), collapse = "\n") %>% unname
},
"x")
