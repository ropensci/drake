# Walkthrough: https://ropenscilabs.github.io/drake-manual/walkthrough.html
# Download the code: drake_example("main") # nolint

# Load your packages and supporting functions into your session.
# If you use supporting scripts like the ones below,
# you will need to supply them yourself. Examples:
# https://github.com/wlandau/drake-examples/tree/master/main/R
source("R/packages.R")  # Load your packages, e.g. library(drake).
source("R/functions.R") # Define your custom code as a bunch of functions.
source("R/plan.R")      # Create your drake plan.

# Call make() to run your work.
# Your targets will be stored in a hidden .drake/ cache,
# and you can read them back into memory with loadd() and read().
make(plan)
