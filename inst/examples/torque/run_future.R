# This script sends work to a TORQUE cluster
# using the future/batchtools backends in drake.
# Deploy it with the following Linux terminal command:
# nohup nice -4 R CMD BATCH run.R &
# That way, a persistent background process will manage the jobs.

library(future.batchtools)
library(drake)

# Create the template file. You may have to modify it.
drake_hpc_template_file("torque_future.tmpl")

# Use future::plan(multicore) instead for a dry run.
future::plan(batchtools_torque, template = "torque_future.tmpl")

load_mtcars_example()
make(my_plan, parallelism = "future", jobs = 4) # transient workers
# make(my_plan, parallelism = "future_lapply", jobs = 4) # persistent workers # nolint
