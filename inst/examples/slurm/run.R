library(future.batchtools)
library(drake)
backend(batchtools_slurm(template = "batchtools.slurm.tmpl"))
load_basic_example()
make(
  my_plan,
  parallelism = "future_lapply"
)
