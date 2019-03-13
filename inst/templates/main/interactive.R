# This script demonstrates how to use the
# r_*() functions described at
# https://ropensci.github.io/drake/reference/r_make.html

# First, define a script called _drake.R (example provided here).
# Then, try it out.

library(drake)
r_outdated()

r_make()

r_outdated()

library(visNetwork) # so the graph renders
r_vis_drake_graph()
