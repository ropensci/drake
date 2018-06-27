# WARNING: FUNCTIONALITY NOT IMPLEMENTED YET.

# This script sends work to a TORQUE cluster
# using drake's clustermq backend.
# Deploy it with the following Linux terminal command:
# nohup nice -4 R CMD BATCH run.R &
# That way, a persistent background process will manage the jobs.

library(drake)

# Create the template file. You may have to modify it.
drake_hpc_template_file("torque_clustermq.tmpl")

# Use future::plan(multicore) instead for a dry run.
options(clustermq.scheduler = "torque", template = "torque_clustermq.tmpl")

load_mtcars_example()
make(my_plan, parallelism = "clustermq", jobs = 4)
