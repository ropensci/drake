# Optionally use this file to send targets
# to different jobs on a cluster.
# Usage from within R:
#
# shell_file() # Copy this file to your working directory.
# make(my_plan, parallelism = "Makefile",
#   prepend = "SHELL=./shell.sh", ...)
#
# Below, you may have to change 'module load R'
# to 'module load R-3.4.2' or similar.
# Just be sure to name the correct environment module.
# See 'module avail' or 'module avail R'.
# 
#!/bin/bash
shift
echo "module load R; $*" | qsub -sync y -cwd -j y
