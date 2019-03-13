# This file serves the r_*() functions (e.g. r_make()) described at
# https://ropenscilabs.github.io/drake-manual/projects.html#safer-interactivity

# Load our packages and supporting functions into our session.
source("R/packages.R")
source("R/functions.R")

# Create the `drake` plan that outlines the work we are going to do.
source("R/plan.R")

# _drake.R must end with a call to `drake_config()`
drake_config(plan)
