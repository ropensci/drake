graph = function(workflow, targets, envir){
  force(envir)
  imports = as.list(envir)
  keys = c(names(imports), workflow$target)
  values = c(imports, workflow$command)
  dependency_list = lapply(values, dependencies) %>% setNames(nm = keys)
  vertices = c(keys, unlist(dependency_list)) %>% unique
  graph = make_empty_graph() + vertices(vertices)
  for(key in keys)
    for(dependency in dependency_list[[key]])
      graph = graph + edge(dependency, key)
  ignore = lapply(targets, function(vertex)
    subcomponent(graph = graph, v = vertex, mode = "in")$name
  ) %>% unlist %>% unique %>% setdiff(x = vertices)
  graph = delete_vertices(graph, v = ignore)
  if(!is_dag(graph)) stop("Workflow is circular (chicken and egg dilemma).")
  graph
}

graphical_dependencies = function(targets, graph)
  adjacent_vertices(graph = graph, v = targets, mode = "in") %>%
    sapply(FUN = names) %>% unlist %>% unique %>% unname
}
