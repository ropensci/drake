#' @title Function drake_tip
#' @description Output a random tip about drake.
#' @export
#' @examples
#' drake_tip()
#' cat(drake_tip())
drake_tip <- function() {
  tips <- c(
    "New functions: a MUCH IMPROVED plot_graph(),
     build_times(), load_basic_example(), outdated(),
     missed(), deps(), dataframes_graph(),
     max_useful_jobs(), and shell_file().",

    "Use example_drake('basic') to generate code files
     for a minimal drake example walkthrough.",

    "See the vignettes at
     https://cran.r-project.org/package=drake/vignettes for
     in-depth tutorials and other thorough documentation.",

    "Use suppressPackageStartupMessages() to eliminate
     package startup messages like this one.",

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
