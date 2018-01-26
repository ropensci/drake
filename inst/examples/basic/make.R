# Run the code in each of the other R scripts in the R folder
# You don't need interactive-tutorial.R, which is independent and separate.

source("R/packages.R")  # Load all the packages you need.
source("R/functions.R") # Load all the functions into your environment.
source("R/plan.R")      # Build your workflow plan data frame.

# Now, your functions and workflow plan should be in your environment.
ls()

# Optionally plot the graph of your workflow.
# config <- drake_config(my_plan) # nolint
# vis_drake_graph(config)         # nolint

# Now it is time to actually run your project.
make(my_plan) # Or make(my_plan, jobs = 2), etc.

# Read the output report.md file
# for an overview of the project and the results.
