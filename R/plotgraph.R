#' @title Function \code{plot_graph}
#' @description Plot the dependency structure of your workflow.
#' IMPORTANT: you must be in the root directory of your project.
#' @export
#' @seealso \code{\link{build_graph}}
#' @param plan workflow plan data frame, same as for function 
#' \code{\link{make}()}.
#' @param targets names of targets to bulid, same as for function
#' \code{\link{make}()}.
#' @param envir environment to import from, same as for function
#' \code{\link{make}()}.
#' @param verbose logical, whether to output messages to the console.
#' @param jobs The \code{outdated()} function is called internally, 
#' and it needs to import objects and examine your 
#' input files to see what has been updated. This could take some time,
#' and parallel computing may be needed
#' to speed up the process. The \code{jobs} argument is number of parallel jobs 
#' to use for faster computation.
#' @param parallelism Choice of parallel backend to speed up the computation.
#' See \code{?parallelism_choices} for details. The Makefile option is not available
#' here. Drake will try to pick the best option for your system by default.
#' @param font_size numeric, font size of the node labels in the graph
#' @param graph an igraph object if one has already been built with 
#' \code{\link{build_graph}()}. 
#' @param navigationButtons logical, whether to add navigation buttons with 
#' \code{visNetwork::visInteraction(navigationButtons = TRUE)}
#' @param ... other arguments passed to \code{visNetwork::visNetwork()} to plot the graph.
#' @examples
#' \dontrun{
#' load_basic_example()
#' plot_graph(my_plan, width = "100%") # The width is passed to visNetwork().
#' make(my_plan)
#' plot_graph(my_plan) # The red nodes from before are now green.
#' }
plot_graph = function(plan, targets = drake::possible_targets(plan), 
  envir = parent.frame(), verbose = FALSE, jobs = 1, 
  parallelism = drake::default_parallelism(), font_size = 20, graph = NULL, 
  navigationButtons = TRUE, ...){
  
  force(envir)
  raw_graph = dataframes_graph(plan = plan, targets = targets, 
     envir = envir, verbose = verbose, jobs = jobs, parallelism = parallelism,
     font_size = font_size, graph = graph)
  
  out = visNetwork(nodes = raw_graph$nodes, edges = raw_graph$edges, ...) %>%
    visLegend(useGroups = FALSE, addNodes = raw_graph$legend_nodes) %>% 
    visHierarchicalLayout(direction = "LR") %>%
    visIgraphLayout(physics = FALSE, randomSeed = 2017) # increases performance
  if(navigationButtons)
    out = visInteraction(out, navigationButtons = TRUE)
  out
}

#' @title Function \code{dataframes_graph}
#' @description Get the information about nodes, edges, and the legend/key
#' so you can plot your own custom \code{visNetwork}.
#' IMPORTANT: you must be in the root directory of your project.
#' @export
#' @return a list of three data frames: one for nodes, one for edges, and one for
#' the legend/key nodes.
#' @seealso \code{\link{plot_graph}}, \code{\link{build_graph}}
#' @param plan workflow plan data frame, same as for function 
#' \code{\link{make}()}.
#' @param targets names of targets to bulid, same as for function
#' \code{\link{make}()}.
#' @param envir environment to import from, same as for function
#' \code{\link{make}()}.
#' @param verbose logical, whether to output messages to the console.
#' @param jobs The \code{outdated()} function is called internally, 
#' and it needs to import objects and examine your 
#' input files to see what has been updated. This could take some time,
#' and parallel computing may be needed
#' to speed up the process. The \code{jobs} argument is number of parallel jobs 
#' to use for faster computation.
#' @param parallelism Choice of parallel backend to speed up the computation.
#' See \code{?parallelism_choices} for details. The Makefile option is not available
#' here. Drake will try to pick the best option for your system by default.
#' @param font_size numeric, font size of the node labels in the graph
#' @param graph an igraph object if one has already been built with 
#' \code{\link{build_graph}()}. 
#' @examples
#' \dontrun{
#' load_basic_example()
#' raw_graph = dataframes_graph(my_plan)
#' str(raw_graph)
#' # Plot your own custom visNetwork graph
#' library(magrittr)
#' library(visNetwork)
#' visNetwork(nodes = raw_graph$nodes, edges = raw_graph$edges) %>%
#'   visLegend(useGroups = FALSE, addNodes = raw_graph$legend_nodes) %>% 
#'   visHierarchicalLayout(direction = "LR")
#' }
dataframes_graph = function(plan, targets = drake::possible_targets(plan), 
   envir = parent.frame(), verbose = FALSE, jobs = jobs, 
   parallelism = drake::default_parallelism(), font_size = 20, graph = NULL){
  force(envir)
  if(is.null(graph))
    graph = build_graph(plan = plan, targets = targets, 
      envir = envir, verbose = verbose)
  
  generic_color = "gray"
  import_color = "#1874cd"
  up_to_date_color = "#228b22"
  outdated_color = "#aa0000"
  missing_color = "#9a32cd"
  generic_shape = "dot"
  file_shape = "square"
  function_shape = "triangle"
  
  network_data = toVisNetworkData(graph)
  nodes = network_data$nodes
  edges = network_data$edges
  if(!nrow(nodes)) return(null_graph())
  
  targets = intersect(nodes$id, plan$target)
  imports = setdiff(nodes$id, plan$target)
  functions = Filter(x = imports, f = function(x) 
    is.function(envir[[x]]) | can_get_function(x))
  missing = Filter(x = imports, f = function(x) missing_import(x, envir = envir))
  
  nodes = resolve_levels(nodes, graph)
  nodes$font.size = font_size
  nodes$status = "import"
  nodes$color = import_color
  nodes[missing, "status"] = "missing"
  nodes[missing, "color"] = missing_color
  
  outdated = outdated(plan = plan, targets = targets, envir = envir, 
    verbose = verbose, jobs = jobs, parallelism = parallelism)
  nodes[targets, "status"] = "up-to-date"
  nodes[targets, "color"] = up_to_date_color
  nodes[outdated, "status"] = "outdated"
  nodes[outdated, "color"] = outdated_color
  
  nodes$shape = generic_shape
  nodes[is_file(nodes$id), "shape"] = file_shape
  nodes[functions, "shape"] = function_shape
  
  if(nrow(edges)) edges$arrows = "to"
  
  legend_nodes = data.frame(
    label = c("Up to date", "Outdated", "Imported", "Missing", 
              "Object", "Function", "File"),
    color = c(up_to_date_color, outdated_color, import_color, missing_color, 
              generic_color, generic_color, generic_color),
    shape = c(generic_shape, generic_shape, generic_shape, generic_shape, generic_shape, 
              function_shape, file_shape),
    font.color = "black",
    font.size = font_size)
  legend_nodes$id = seq_len(nrow(legend_nodes))
  
  list(nodes = nodes, edges = edges, legend_nodes = legend_nodes) 
}

can_get_function = function(x){
  tryCatch({
    is.function(get(x))
  }, error = function(e) FALSE)
}

null_graph = function(){
  nodes = data.frame(id = 1, label = "Nothing to plot.")
  visNetwork(nodes = nodes, edges = data.frame(from = NA, to = NA))
}

missing_import = function(x, envir){
  missing_object = !is_file(x) & 
    is.null(envir[[x]]) & 
    tryCatch({tmp = get(x); FALSE}, error = function(e) TRUE)
  missing_file = is_file(x) & !file.exists(unquote(x))
  missing_object | missing_file
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
