library(future)
library(drake)

cl <- future::makeClusterPSOCK( # nolint
  "localhost",
  ## Launch Rscript inside Docker container
  rscript = c(
    "docker", "run", "--net=host", "rocker/r-base",
    "Rscript"
  ),
  ## Install drake
  rscript_args = c(
    "-e", shQuote("install.packages('drake')")
  )
)

backend(cluster, workers = cl)
load_basic_example()
make(my_plan, parallelism = "future_lapply")
