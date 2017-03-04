###############
### PURPOSE ###
###############
#
# Show how to use drake to reproducibly manage 
# and accelerate a data analysis workflow.
#
# Enter vignette("quickstart") for a walkthrough
# of this code.
#
###################################
### LOAD PACKAGES AND FUNCTIONS ###
###################################

library(knitr)
library(rmarkdown)
library(drake)

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
  large = simulate(50))

# Optionally, get replicates with expand(datasets,
#   values = c("rep1", "rep2")).

methods = plan(
  regression1 = reg1(..dataset..),
  regression2 = reg2(..dataset..))

# same as evaluate(plan, wildcard = "..dataset..",
#   values = datasets$output)
analyses = analyses(methods, datasets = datasets)

summary_types = plan(summ = summary(..analysis..),
                     coef = coef(..analysis..))

# summaries() also uses evaluate(): once with expand = TRUE,
#   once with expand = FALSE
results = summaries(summary_types, analyses, datasets, 
  gather = NULL) # skip 'gather' (workflow plan is more readable)

load_in_report = plan(
  report_dependencies = c(small, large, coef_regression2_small))

# External file dependencies should be single-quoted.
report = plan(
  report.md = my_knit('report.Rmd', report_dependencies),
  report.html = my_render('report.md', report_dependencies),
  file_targets = TRUE, strings_in_dots = "filenames")

# Row order doesn't matter in the workflow plan.
plan = rbind(report, datasets, load_in_report, analyses, results)

################################
### SINGLE-PROCESS EXECUTION ###
################################

# Start off with a clean workspace (optional).
clean() # Cleans out the hidden cache in the .drake/ folder if it exists.

# Use make() to execute your workflow. 
# These functions are exactly the same.
make(plan) # build everything from scratch
# Now, open and read report.html in a browser.
readd(coef_regression2_large) # see also: loadd(), cached()

# Everything is up to date.
make(plan)

# Change to a cubic term and rerun.
reg2 = function(d){
  d$x3 = d$x^3
  lm(y ~ x3, data = d)
}
make(plan) # Drake only runs targets that depend on reg2().

#########################################
### NEED TO ADD MORE WORK ON THE FLY? ###
#########################################

# Just write more functions and add rows to your workflow plan.
new_simulation = function(n){
  data.frame(x = rnorm(n), y = rnorm(n))
}

# Any R expression can be a command 
# except for formulas and function definitions.
additions = plan(
  new_data = new_simulation(36) + sqrt(10))  

# Add the new work
plan = rbind(plan, additions)
make(plan) # Only the new work is run.

# Clean up and start over next time.
# Use clean(small), clean(list = "large"), etc. 
# to remove individual targets.
clean() # report.html and report.md are removed, but report.Rmd stays.

###############################################
### ONE R SESSION WITH 2 PARALLEL PROCESSES ###
###############################################

# Does not work on Windows.
make(plan, parallelism = "mclapply", jobs = 2) # "mclapply" is default.
readd(coef_regression2_large) # see also: loadd(), cached()

# All up to date.
make(plan, jobs = 2)
clean() # Start over next time.

######################################################
### DISTRIBUTED COMPUTING: TWO PARALLEL R SESSIONS ###
######################################################

# Write a Makefile and execute it to spawn up to two
# R sessions at a time.
make(plan, parallelism = "Makefile", jobs = 2) # build everything
readd(coef_regression2_large) # see also: loadd(), cached()

# Drake tells the Makefile what is already up to date.
make(plan, parallelism = "Makefile", jobs = 2)
clean() # Start over next time.

######################################################
### DISTRIBUTED COMPUTING: FOUR NODES ON A CLUSTER ###
######################################################

# The file shell.sh tells the Makefile to submit jobs on a cluster.
# You could write this file by hand if you wanted.
# You may have to change 'module load R/3.3.2' to another 
# version of R that has your packages.

writeLines(c(
  "#!/bin/bash",
  "shift",
  "echo \"module load R/3.3.2; $*\" | qsub -sync y -cwd -j y"
), "shell.sh")

system2("chmod", args = c("+x", "shell.sh")) # permission to execute

# In reality, you would put all your code in an R script
# and then run it in the Linux/Mac terminal with
# nohup nice -19 R CMD BATCH my_script.R &

# Run up to four parallel jobs on the cluster or supercomputer,
# depending on what is needed. These jobs could go to multiple 
# nodes for true distributed computing.
make(plan, parallelism = "Makefile", jobs = 4, # build
  prepend = "SHELL=./shell.sh")
readd(coef_regression2_large) # see also: loadd(), cached()

# Everything is up to date, so no jobs should be submitted... 
# that is, unless your file system is slow to 
# sync the nodes on the cluster with your working directory.
# In that case, drake might not see the most up-to-date 
# information in its cache and it may submit unnecessary jobs.
make(plan, parallelism = "Makefile", jobs = 4, 
  prepend = "SHELL=./shell.sh")

###########################
### CLEAN UP ALL OUTPUT ###
###########################

clean(destroy = TRUE) # Totally remove the hidden .drake/ cache.
unlink(c("Makefile", "report.Rmd", "shell.sh", "STDIN.o*")) # Clean up other files.
