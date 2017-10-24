library(future.batchtools)
library(drake)
backend(batchtools_slurm(template = "sge-simple.tmpl"))
load_basic_example()
make(
  my_plan,
  parallelism = "future_lapply"
)
