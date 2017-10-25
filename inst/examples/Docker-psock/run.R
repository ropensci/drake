library(future)
library(drake)

## Setup of Docker worker running rocker/r-base
## (requires installation of future package)
cl <- makeClusterPSOCK(
  "localhost",
  ## Launch Rscript inside Docker container
  rscript = c(
    "docker", "run", "--net=host", "rocker/r-base",
    "Rscript"
  ),
  ## Install future package
  rscript_args = c(
    "-e", shQuote("install.packages('future')")
  ),
  dryrun = TRUE
)

backend(cluster, workers = cl)
load_basic_example()
make(my_plan, parallelism = "future_lapply")
