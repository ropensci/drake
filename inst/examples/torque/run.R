# Deploy this script with the following Linux terminal command:
# nohup nice -4 R CMD BATCH run.R &
# That way, a persistent background process will manage the jobs.

library(future.batchtools)
library(drake)
backend(batchtools_torque(template = "torque.tmpl"))
load_basic_example(overwrite = FALSE)
make(
  my_plan,
  parallelism = "future_lapply"
)
