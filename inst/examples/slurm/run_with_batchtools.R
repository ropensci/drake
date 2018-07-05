# This script sends work to a SLURM cluster
# using the future/batchtools backends in drake.
# Deploy it with the following Linux terminal command:
# nohup nice -4 R CMD BATCH run.R &
# That way, a persistent background process will manage the jobs.

library(future.batchtools)
library(drake)

# Create the template file. You may have to modify it.
drake_hpc_template_file("slurm_batchtools.tmpl")

# Use future::plan(multicore) instead for a dry run.
future::plan(batchtools_slurm, template = "slurm_batchtools.tmpl")

load_mtcars_example()
make(my_plan, parallelism = "future", jobs = 4) # transient workers
# make(my_plan, parallelism = "future_lapply", jobs = 4) # persistent workers # nolint
# make(my_plan, parallelism = "future_lapply_staged", jobs = 4) # staged scheduling # nolint
