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

# To skip to the "CHECK AND DEBUG WORKFLOW PLAN" section, just
# call load_basic_example().

library(knitr) # Drake knows you loaded knitr.
library(drake)

clean() # remove any previous drake output

# User-defined functions
simulate <- function(n){
  data.frame(
    # Drake tracks calls like `pkg::fn()` (namespaced functions).
    x = stats::rnorm(n),
    y = rpois(n, 1)
  )
}

reg1 <- function(d){
  lm(y ~ + x, data = d)
}

reg2 <- function(d){
  d$x2 <- d$x ^ 2
  lm(y ~ x2, data = d)
}

# At this point, please verify that a dynamic report
# called report.Rmd is in your working directory.

###############################
### CONSTRUCT WORKFLOW PLAN ###
###############################

# To skip to the "CHECK AND DEBUG WORKFLOW PLAN" section, just
# call load_basic_example().

my_datasets <- workplan(
  small = simulate(5),
  large = simulate(50)
)

# Optionally, get replicates with expand(my_datasets,
#   values = c("rep1", "rep2")).

methods <- workplan(
  regression1 = reg1(dataset__),
  regression2 = reg2(dataset__)
)

# same as evaluate(methods, wildcard = "..dataset..",
#   values = my_datasets$target)
my_analyses <- plan_analyses(methods, datasets = my_datasets)

summary_types <- workplan(
  # Perfect regression fits can happen.
  summ = suppressWarnings(summary(analysis__)),
  coef = coefficients(analysis__)
)

# summaries() also uses evaluate(): once with expand = TRUE,
#   once with expand = FALSE
results <- plan_summaries(
  summary_types,
  my_analyses,
  my_datasets,
  gather = NULL
) # skip 'gather' (workflow my_plan is more readable)

# External file targets and dependencies should be single-quoted.
# Use double quotes to remove any special meaning from character strings.
# Single quotes inside imported functions are ignored, so this mechanism
# only works inside the workflow my_plan data frame.
# WARNING: drake cannot track entire directories (folders).
report <- workplan(
  # As long as `knit()` is visible in your workflow plan command,
  # drake will dig into the active code chunks of your `report.Rmd`
  # and find the dependencies of `report.md` in the arguments of
  # calls to loadd() and readd().
  report.md = knit(
    'report.Rmd', #nolint: use single quotes to specify file dependency.
     quiet = TRUE
  ),
  file_targets = TRUE,
  strings_in_dots = "filenames" # Redundant, since we used single quotes
)

# Row order doesn't matter in the workflow my_plan.
my_plan <- rbind(report, my_datasets, my_analyses, results)


#####################################
### CHECK AND DEBUG WORKFLOW PLAN ###
#####################################

# Graph the dependency structure of your workflow
# vis_drake_graph(my_plan) # plots an interactive web app via visNetwork. #nolint optional
workflow_graph <- build_drake_graph(my_plan) # igraph object

# Check for circularities, missing input files, etc.
check_plan(my_plan)

# Check the dependencies of individual functions and commands.
deps(reg1)
deps(my_plan$command[1])
deps(my_plan$command[nrow(my_plan)])

# List objects that are reproducibly tracked.
"small" %in% tracked(my_plan)
tracked(my_plan, targets = "small")
tracked(my_plan)

# See vignette("caution") for more a deeper dive into possible pitfalls.


################################
### SINGLE PROCESS EXECUTION ###
################################

# Start off with a clean workspace (optional).
clean() # Cleans out the hidden cache in the .drake/ folder if it exists.

# All the targets in the plan are "outdated" because we have not made them yet.
outdated(my_plan, verbose = FALSE)
# vis_drake_graph(my_plan) # Show how the pieces of your workflow are connected #nolint: optional
missed(my_plan) # Nothing should be missing from your workspace.

make(my_plan) # Run your project.
# The non-file dependencies of your last target are already loaded
# in your workspace.

# How long did each target take to build?
build_times()

ls() # Should contain the non-file dependencies of the last target(s).
progress() # See also in_progress()
outdated(my_plan, verbose = FALSE) # Everything is up to date
# vis_drake_graph(my_plan) # The red nodes from before turned green. #nolint: optional
# session() # get the sessionInfo() of the last call to make() #nolint: optional

# see also: loadd(), cached(), imported(), and built()
readd(coef_regression2_large) # Read target from the cache.

# Everything is up to date.
make(my_plan)

# Change to a cubic term and rerun.
reg2 <- function(d){
  d$x3 <- d$x ^ 3
  lm(y ~ x3, data = d)
}
outdated(my_plan) # The targets depending on reg2() are now out of date...
# vis_drake_graph(my_plan) # ...which is indicated in the graph. #nolint: optional

make(my_plan) # Drake only runs targets that depend on reg2().

# For functions and my_plan$command,
# trivial changes like comments and whitespace are ignored.
reg2 <- function(d){
  d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d) # I indented here.
}
outdated(my_plan) # Everything is still up to date.

#########################################
### NEED TO ADD MORE WORK ON THE FLY? ###
#########################################

# Just write more functions and add rows to your workflow plan.
new_simulation <- function(n){
  data.frame(x = rnorm(n), y = rnorm(n))
}

# Any R expression can be a command
# except for formulas and function definitions.
additions <- workplan(
  new_data = new_simulation(36) + sqrt(10)
  )

# Add the new work
my_plan <- rbind(my_plan, additions)
make(my_plan) # Only the new work is run.

# Clean up and start over next time.
# Use clean(small), clean(list = "large"), etc.
# to remove individual targets.
clean() # report.html and report.md are removed, but report.Rmd stays.

# Garbage collection
drake_gc() # Also consider clean(garbage_collection = TRUE)

###############################################
### ONE R SESSION WITH 2 PARALLEL PROCESSES ###
###############################################

# How many parallel jobs might be useful?
# At what point would it be ridiculous to add more jobs?
max_useful_jobs(my_plan)

make(my_plan, jobs = 2) # parallelism == "parLapply" for Windows
# make(my_plan, parallelism = "mclapply", jobs = 2) # Not for Windows #nolint: optional
readd(coef_regression2_large) # see also: loadd(), cached()

# All up to date.
make(my_plan, jobs = 2)
clean() # Start over next time.

######################################
### PARALLEL COMPUTING WITH FUTURE ###
######################################

# The `future` and `future.batchtools` packages
# unlock a huge array of powerful parallel backends.
# Here is just a taste. You can find a list of
# future.batchtools backends at
# https://github.com/HenrikBengtsson/future.batchtools#choosing-batchtools-backend # nolint
# Note: the `jobs` does not apply to the "future_lapply" backend.
# Use `options(mc.cores = 4)` or something similar from ?future.options
# to cap the number of simultaneous jobs.
options(mc.cores = 2)
library(future)
future::plan(multicore) # Avoid drake::plan().
make(my_plan, parallelism = "future_lapply")
clean() # Erase the targets to start from scratch.

future::plan(multisession) # Use separate background R sessions.
make(my_plan, parallelism = "future_lapply")
clean()

if (require(future.batchtools)){ # More heavy-duty future-style parallel backends # nolint
  future::plan(batchtools_local)
  make(my_plan, parallelism = "future_lapply")
  clean()

  # Deploy targets with batchtools_local and use `future`-style
  # multicore parallism each individual target's command.
  future::plan(list(batchtools_local, multicore))
  make(my_plan, parallelism = "future_lapply")
  clean()
}
clean() # Start over next time

######################################################
### DISTRIBUTED COMPUTING: TWO PARALLEL R SESSIONS ###
######################################################

# Write a Makefile and execute it to spawn up to two
# R sessions at a time.
# Windows users need Rtools (https://cran.r-project.org/bin/windows/Rtools)
# Everyone else just needs Make (https://www.gnu.org/software/make)
# or an equivalent program.
make(my_plan, parallelism = "Makefile", jobs = 2) # build everything
readd(coef_regression2_large) # see also: loadd(), cached()

# Drake tells the Makefile what is already up to date.
make(my_plan, parallelism = "Makefile", jobs = 2)
clean() # Start over next time.

######################################################
### DISTRIBUTED COMPUTING: FOUR NODES ON A CLUSTER ###
### ONLY ATTEMPT ON A PROPER COMPUTING CLUSTER     ###
######################################################

# Use FALSE on regular local machines.
if (FALSE){
# if (TRUE){ # Only attempt this part on a proper computing cluster.

  # The file shell.sh tells the Makefile to submit jobs on a cluster.
  # You could write this file by hand if you want, or you can
  # generate a starter file with drake::shell_file().
  # You may have to change 'module load R' to a command that
  # loads a specific version of R.
  # Writes an example shell.sh and does a `chmod +x` so drake can execute it.

  shell_file()

  # In reality, you would put all your code in an R script
  # and then run it in the Linux/Mac terminal with
  # nohup nice -19 R CMD BATCH my_script.R &

  # Run up to four parallel jobs on the cluster or supercomputer,
  # depending on what is needed. These jobs could go to multiple
  # nodes for true distributed computing.
  make(my_plan, parallelism = "Makefile", jobs = 4, # build
    prepend = "SHELL=./shell.sh") # Generate shell.sh with shell_file().

  # Alternatively, users of SLURM (https://slurm.schedmd.com/)
  # can just point to `srun` and dispense with `shell.sh` altogether.
  # make(my_plan, parallelism = "Makefile", jobs = 4,
  #   prepend = "SHELL=srun")

  readd(coef_regression2_large) # see also: loadd(), cached()

  # Everything is up to date, so no jobs should be submitted.
  make(
    my_plan,
    parallelism = "Makefile",
    jobs = 4,
    prepend = "SHELL=./shell.sh"
  )
} # if(FALSE)

###########################
### CLEAN UP ALL OUTPUT ###
###########################

clean(destroy = TRUE) # Totally remove the hidden .drake/ cache.
unlink(
  c(
    ".future",
    "Makefile",
    "shell.sh",
    "STDIN.o*"
  ),
  recursive = TRUE
) # Clean up other files.
