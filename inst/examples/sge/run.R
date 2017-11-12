# Deploy this script with the following Linux terminal command:
# nohup nice -4 R CMD BATCH run.R &
# That way, a persistent background process will manage the jobs.

library(future.batchtools)
library(drake)

# Use backend(multicore) instead for a dry run.
future::plan(batchtools_sge(template = "sge-simple.tmpl"))

load_basic_example()
make(
  my_plan,
  parallelism = "future_lapply"
)
