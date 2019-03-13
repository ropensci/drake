# Walkthrough: https://ropenscilabs.github.io/drake-manual/intro.html
# Slides: https://krlmlr.github.io/drake-pitch
# Code: drake_example("main") # nolint

# Load our packages and supporting functions into our session.
source("R/packages.R")
source("R/functions.R")

# Create the `drake` plan that outlines the work we are going to do.
source("R/plan.R")

# Run your work with make().
make(plan)

# See also loadd(), readd(), vis_drake_graph(), and drake_config(). # nolint
