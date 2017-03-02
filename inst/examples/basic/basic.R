###############
### PURPOSE ###
###############
#
# Show how to use drake to reproducibly manage 
# and accelerate a data analysis workflow.
#
###################################
### LOAD PACKAGES AND FUNCTIONS ###
###################################

library(drake)
library(knitr)
library(rmarkdown)

clean() # remove previous drake output

# User-defined functions
simulate = function(n){
  data.frame(
    x = rnorm(n),
    y = rpois(n, 1)
  )
}

reg1 = function(d){
  lm(y ~ + x, data = d)
}

reg2 = function(d){
  d$x2 = d$x^2
  lm(y ~ x2, data = d)
}

# Knit and render a dynamic knitr report
my_knit = function(file, ...){
  knit(file) # drake knows you loaded the knitr package
}

my_render = function(file, ...){
  render(file) # drake knows you loaded the rmarkdown package
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

###############################
### CONSTRUCT WORKFLOW PLAN ###
###############################

datasets = plan(
  small = simulate(5),
  large = simulate(50)
)

# Optionally, get replicates with expand(datasets,
#   values = c("rep1", "rep2")).

methods = plan(
  regression1 = reg1(..dataset..),
  regression2 = reg2(..dataset..)
)

# same as evaluate(plan, wildcard = "..dataset..",
#   values = datasets$output)
analyses = analyses(methods, data = datasets)

summary_types = plan(summ = summary(..analysis..),
                     coef = coef(..analysis..))

# summaries() also uses evaluate(): once with expand = TRUE,
#   once with expand = FALSE
results = summaries(summary_types, analyses, datasets, 
  gather = NULL) # for now, skip 'gather' so the `plan` is more readable

load_in_report = plan(
  report_dependencies = c(small, large, coef_regression2_small))

# External file dependencies should be single-quoted.
report = plan(
  report.md = my_knit('report.Rmd', report_dependencies),
  report.html = my_render('report.md', report_dependencies),
  file_outputs = TRUE, strings_in_dots = "filenames")

# Row order doesn't matter in the workflow plan.
plan = rbind(report, datasets, load_in_report, analyses, results)

################################
### SINGLE-PROCESS EXECUTION ###
################################

run(plan) # build everything from scratch
# Now, open and read report.html in a browser.
readd(coef_regression2_large) # see also: loadd(), cached()
run(plan) # everything is up to date
reg2 = function(d){ # change to cubic term
  d$x3 = d$x^3
  lm(y ~ x3, data = d)
}
run(plan) # drake regenerates the affected results only
clean() # Start over next time: report.html is gone, but report.Rmd stays.

###############################################
### ONE R SESSION WITH 2 PARALLEL PROCESSES ###
###############################################

run(plan, parallelism = "single-session", jobs = 2) # does not work on Windows
readd(coef_regression2_large) # see also: loadd(), cached()
run(plan, parallelism = "single-session", jobs = 2) # all up to date
clean() # start over next time

####################################################
### DISTRIBUTED COMPUTING: 2 PARALLEL R SESSIONS ###
####################################################

run(plan, parallelism = "distributed", jobs = 2) # build
readd(coef_regression2_large) # see also: loadd(), cached()
run(plan, parallelism = "distributed", jobs = 2) # all up to date
clean() # start over next time

###################################################
### DISTRIBUTED COMPUTING: 4 NODES ON A CLUSTER ###
###################################################

# The file shell.sh tells the Makefile to submit jobs on a cluster.
# You could write this file by hand if you wanted.
# You may have to change 'module load R/3.3.2' to another 
# version of R that has your packages.

writeLines(c(
  "#!/bin/bash",
  "shift",
  "echo \"module load R/3.3.2; $*\" | qsub -sync y -cwd -j y"
), "shell.sh")

system("chmod +x shell.sh") # permission to execute

# put inside my_script.R and run:
#   nohup nice -19 R CMD BATCH my_script.R &
run(plan, parallelism = "distributed", jobs = 4, # build
  prepend = "SHELL=./shell.sh")
readd(coef_regression2_large) # see also: loadd(), cached()
# all up to date
run(plan, parallelism = "distributed", jobs = 4, 
  prepend = "SHELL=./shell.sh")

###########################
### CLEAN UP ALL OUTPUT ###
###########################

clean(destroy = TRUE) # totally remove the '.drake/' cache
unlink(c("Makefile", "report.Rmd", "shell.sh")) # clean up other files
system("rm -f STDIN.o*")
