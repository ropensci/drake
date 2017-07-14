#' @title Function \code{example_drake}
#' @description Copy a folder of code files for a 
#' drake example to the current working directory.
#' To see the names of all the examples, run \code{\link{examples_drake}}.
#' @seealso \code{\link{examples_drake}}, \code{\link{make}}
#' @export
#' @param example name of the example. 
#' To see all the available example names, 
#' run \code{\link{examples_drake}}.
#' @param destination character scalar, file path, where
#' to write the folder containing the code files for the example.
#' @examples
#' \dontrun{
#' example_drake("basic")
#' }
example_drake = function(example = drake::examples_drake(), 
  destination = getwd()){
  example = match.arg(example)
  dir = system.file(file.path("examples", example), 
    package = "drake", mustWork = TRUE)
  if(file.exists(example)) 
    stop("There is already a file or folder named ", 
      example, ".", sep = "")
  file.copy(from = dir, to = destination, recursive = TRUE)
  invisible()
}

#' @title Function \code{examples_drake}
#' @description List the names of all the drake examples.
#' @export
#' @seealso \code{\link{example_drake}}, \code{\link{make}}
#' @return names of all the drake examples.
#' @examples
#' examples_drake()
examples_drake = function(){
  list.dirs(system.file("examples", package = "drake", 
    mustWork = TRUE), full.names = FALSE, recursive = FALSE)
}

#' @title Function \code{load_basic_example}
#' @description Load the basic example into your workspace (or the environment you specify).
#' @export
#' @param envir The environment to load the example into. Defaults to your workspace.
#' For an insulated workspace, set \code{envir = new.env(parent = globalenv())}.
#' @examples
#' \dontrun{
#' load_basic_example()
#' deps(reg1)
#' deps(my_plan$command[1])
#' deps(my_plan$command[4])
#' plot_graph(my_plan)
#' make(my_plan)
#' clean(destroy = TRUE)
#' unlink("report.Rmd")
#' }
load_basic_example = function(envir = parent.frame()){
  require(knitr, quietly = TRUE)
  require(drake, quietly = TRUE)
  
  # User-defined functions
  envir$simulate = function(n){
    data.frame(
      x = rnorm(n),
      y = rpois(n, 1)
    )
  }
  
  envir$reg1 = function(d){
    lm(y ~ + x, data = d)
  }
  
  envir$reg2 = function(d){
    d$x2 = d$x^2
    lm(y ~ x2, data = d)
  }
  
  # Knit and render a dynamic knitr report
  envir$my_knit = function(file, ...){
    knit(file) # drake knows you loaded the knitr package
  }
  
  # Write the R Markdown source for a dynamic knitr report
  lines = c(
    "---",
    "title: Example Report",
    "author: You",
    "output: html_document",
    "---",
    "",
    "Look how I read outputs from the drake cache.",
    "",
    "```{r}",
    "library(drake)",
    "readd(small)",
    "readd(coef_regression2_small)",
    "loadd(large)",
    "head(large)",
    "```"
  )
  
  writeLines(lines, "report.Rmd")

  # construct workflow plan
  
  datasets = plan(
    small = simulate(5),
    large = simulate(50))


  methods = plan(
    regression1 = reg1(..dataset..),
    regression2 = reg2(..dataset..))

  # same as evaluate(my_plan, wildcard = "..dataset..",
  #   values = datasets$output)
  analyses = analyses(methods, datasets = datasets)

  summary_types = plan(
    summ = summary(..analysis..),
    coef = coef(..analysis..))

  # summaries() also uses evaluate(): once with expand = TRUE,
  #   once with expand = FALSE
  results = summaries(summary_types, analyses, datasets, 
    gather = NULL) # skip 'gather' (workflow my_plan is more readable)

  load_in_report = plan(
    report_dependencies = c(small, large, coef_regression2_small))

  # External file targets and dependencies should be single-quoted.
  # Use double quotes to remove any special meaning from character strings.
  # Single quotes inside imported functions are ignored, so this mechanism
  # only works inside the workflow my_plan data frame.
  # WARNING: drake cannot track entire directories (folders).
  report = plan(
    report.md = my_knit('report.Rmd', report_dependencies),
    file_targets = TRUE, strings_in_dots = "filenames")

  # Row order doesn't matter in the workflow my_plan.
  envir$my_plan = rbind(report, datasets, load_in_report, analyses, results)
  invisible(my_plan)
}
