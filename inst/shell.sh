# Write this file with shell_file().
#
# Optionally use this file to send targets
# to different jobs on a cluster.
#
# library(drake)
# ...
# shell_file() # copy this file to your working directory.
# make(my_plan, parallelism = "Makefile",
#   prepend = "SHELL=./shell.sh", ...)
#
#!/bin/bash
shift
echo "module load R; $*" | qsub -sync y -cwd -j y
