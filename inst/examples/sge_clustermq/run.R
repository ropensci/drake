# Deploy this script with the following Linux terminal command:
# nohup nice -4 R CMD BATCH run.R &
# That way, a persistent background process will manage the jobs.

# Tell clustermq to use the SGE scheduler
# with the configuration options in sge-clustermq.tmpl.
options(clustermq.scheduler = "sge", clustermq.template = "sge-clustermq.tmpl")

# Run the project.
library(drake)
load_mtcars_example()
make(my_plan, parallelism = "clustermq_persistent", jobs = 4)
