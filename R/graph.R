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
  keys = c(names(imports), plan$target)
  values = c(imports, plan$command)
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
  if(!is_dag(graph)) 
    stop("Workflow is circular (chicken and egg dilemma).")
  graph
}

graphical_dependencies = function(targets, args){
  adjacent_vertices(graph = args$graph, v = targets, mode = "in") %>%
    lapply(FUN = names) %>% unlist %>% unique %>% unname
}
