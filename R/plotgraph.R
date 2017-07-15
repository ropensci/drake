#' @title Function \code{plot_graph}
#' @description Plot the dependency structure of your workflow
#' @export
#' @seealso \code{\link{build_graph}}
#' @param plan workflow plan data frame, same as for function 
#' \code{\link{make}()}.
#' @param targets names of targets to bulid, same as for function
#' \code{\link{make}()}.
#' @param envir environment to import from, same as for function
#' \code{\link{make}()}.
#' @param verbose logical, whether to output messages to the console.
#' @param font_size numeric, font size of the node labels in the graph
#' @param graph an igraph object if one has already been built with 
#' \code{\link{build_graph}()}. 
#' @param ... arguments passed to \code{visNetwork::visNetwork()} to plot the graph.
#' @examples
#' \dontrun{
#' load_basic_example()
#' plot_graph(my_plan)
#' }
plot_graph = function(plan, targets = drake::possible_targets(plan), 
                      envir = parent.frame(), verbose = TRUE, 
                      font_size = 20, graph = NULL, ...){
  force(envir)
  if(is.null(graph))
    graph = build_graph(plan = plan, targets = targets, 
      envir = envir, verbose = verbose)
  
  generic_color = "gray"
  import_color = "#1874cd"
  target_color = "#228b22"
  notfound_color = "#9a32cd"
  generic_shape = "dot"
  file_shape = "square"
  function_shape = "triangle"

  network_data = toVisNetworkData(graph)
  nodes = network_data$nodes
  edges = network_data$edges
  if(!nrow(nodes)) return(null_graph())
  
  targets = intersect(nodes$id, plan$target)
  imports = setdiff(nodes$id, plan$target)
  functions = Filter(f = function(x) is.function(envir[[x]]), x = imports)
  notfound = Filter(x = imports,
                    f = function(x) 
                      is.null(envir[[x]]) & 
                      !is_file(x) & 
                      tryCatch({tmp = get(x); FALSE}, error = function(e) TRUE))

  nodes = resolve_levels(nodes, graph)
  nodes$font.size = font_size
  nodes$color = import_color
  nodes[notfound, "color"] = notfound_color
  nodes[targets, "color"] = target_color
  
  nodes$shape = generic_shape
  nodes[is_file(nodes$id), "shape"] = file_shape
  nodes[functions, "shape"] = function_shape

  if(nrow(edges)) edges$arrows = "to"
  
  legend_nodes = data.frame(
    label = c("Target to build", "Item to import", "Cannot import", 
              "Generic object", "Function", "External file"),
    color = c(target_color, import_color, notfound_color, generic_color, generic_color, generic_color),
    shape = c(generic_shape, generic_shape, generic_shape, generic_shape, function_shape, file_shape),
    font.color = "black",
    font.size = font_size)
  legend_nodes$id = seq_len(nrow(legend_nodes))

  visNetwork(nodes = nodes, edges = edges, ...) %>%
    visLegend(useGroups = FALSE, addNodes = legend_nodes) %>% 
    visHierarchicalLayout(direction = "LR")
}

null_graph = function(){
  nodes = data.frame(id = 1, label = "Nothing to plot.")
  visNetwork(nodes = nodes, edges = data.frame(from = NA, to = NA))
}

resolve_levels = function(nodes, graph){
  stopifnot(is_dag(graph))
  level = 1
  nodes$level = NA
  graph_remaining_targets = graph
  while(length(V(graph_remaining_targets))){
    candidates = next_targets(graph_remaining_targets)
    nodes[candidates, "level"] = level
    level = level + 1
    graph_remaining_targets = delete_vertices(graph_remaining_targets, v = candidates)
  }
  stopifnot(all(!is.na(nodes$level)))
  nodes
}
