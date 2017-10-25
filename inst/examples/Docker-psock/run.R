library(future)
library(drake)

cl <- makeClusterPSOCK( # nolint
  "localhost",
  ## Launch Rscript inside Docker container
  rscript = c(
    "docker", "run", "--net=host", "rocker/r-base",
    "Rscript"
  ),
  ## Install drake
  rscript_args = c(
    "-e", shQuote("install.packages('drake')")
  ),
  dryrun = TRUE
)

backend(cluster, workers = cl)
load_basic_example()
make(my_plan, parallelism = "future_lapply")
