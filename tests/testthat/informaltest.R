devtools::load_all()
rm(list = ls())
workflow = dbug()
envir = environment()
targets = workflow$target
graph = graph(workflow = workflow, targets = targets, envir = envir)
