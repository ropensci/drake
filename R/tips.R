#' @title Function drake_tip
#' @description Output a random tip about drake.
#' @export
#' @examples
#' drake_tip()
#' cat(drake_tip())
drake_tip = function(){
  tips = c(
    "New in drake 3.1.0: load_basic_example(), deps(), and a much improved plot_graph().",
    "Use example_drake('basic') to generate code files for a minimal drake example walkthrough.",
    "See the vignettes at https://cran.r-project.org/package=drake/vignettes for in-depth tutorials and other thorough documentation.",
    "Use suppressPackageStartupMessages() to eliminate package startup messages like this one.",
    "Drake quickstart: load_basic_example(); plot_graph(my_plan); make(my_plan); plot_graph(my_plan)"
  )
  tip = sample(tips, 1)
  paste(strwrap(tip), collapse = "\n")
}

drake_message = function(){
  packageStartupMessage(drake_tip())
}
