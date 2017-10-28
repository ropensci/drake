# Deploy this script with the following Linux terminal command:
# nohup nice -4 R CMD BATCH run.R &
# That way, a persistent background process will manage the jobs.

library(drake)
load_basic_example() # Writes `report.Rmd` and sets up your workspace.
# shell_file() # Writes an example `shell.sh` file. # nolint
make(
  my_plan,
  parallelism = "Makefile",
  jobs = 4,
  prepend = "SHELL=./shell.sh"
)
