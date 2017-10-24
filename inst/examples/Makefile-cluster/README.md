This example demonstrates how to deploy targets to separate
jobs on a cluster using "Makefile" parallelism.
`make(..., parallelism = "Makefile")` creates a `Makefile`,
and the `shell.sh` file tells the `Makefile` to send the
targets to the cluster. You may have to configure
your `shell.sh` for your particular cluster. 
You may also need to install [Rtools](https://cran.r-project.org/bin/windows/Rtools/).
When you are ready, run `run.R` to run the example.
