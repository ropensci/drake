#' @title Function \code{graph}
#' @description Graph the dependency structure of your workflow
#' @export
#' @param plan workflow plan data frame
#' @param targets targets to bulid
#' @param envir environment to use
graph = function(plan, targets = plan$target, envir = parent.frame()){
  force(envir)
  build_graph(plan = plan, targets = targets, envir = envir) %>%
    plot.igraph
}

build_graph = function(plan, targets, envir){
  force(envir)
  imports = as.list(envir)
  import_deps = lapply(imports, import_dependencies)
  command_deps = lapply(plan$command, command_dependencies)
  names(command_deps) = plan$target
  dependency_list = c(command_deps, import_deps)
  keys = names(dependency_list)
  vertices = c(keys, unlist(dependency_list)) %>% unique
  graph = make_empty_graph() + vertices(vertices)
  for(key in keys)
    for(dependency in dependency_list[[key]])
      graph = graph + edge(dependency, key)
  ignore = lapply(targets, function(vertex)
    subcomponent(graph = graph, v = vertex, mode = "in")$name
  ) %>% unlist %>% unique %>% setdiff(x = vertices)
  graph = delete_vertices(graph, v = ignore)
  if(!is_dag(graph)) 
    stop("Workflow is circular (chicken and egg dilemma).")
  graph
}
