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
#' Execution order in \code{\link{make}()} is slightly different when 
#' \code{parallelism} equals \code{"Makefile"}
#' because in that case, all the imports are imported before any target is built. 
#' Thus, the arrangement in the graph is different for Makefile parallelism.
#' See \code{?parallelism_choices} for details.
#' @param font_size numeric, font size of the node labels in the graph
#' @param packages same as for \code{\link{make}}
#' @param prework same as for \code{\link{make}}
#' @param build_times logical, whether to show the \code{\link{build_times}()}
#' of the targets, if available.
#' @param targets_only logical, whether to skip the imports and only include the 
#' targets in the workflow plan.
#' @param config option internal runtime parameter list of 
#' \code{\link{make}(...)},
#' produced with \code{\link{config}()}.
#' Computing this
#' in advance could save time if you plan multiple calls to 
#' \code{dataframes_graph()}.
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
                            envir = parent.frame(), verbose = TRUE, jobs = 1, 
                            parallelism = drake::default_parallelism(), 
                            packages = (.packages()), prework = character(0),
                            build_times = TRUE, targets_only = FALSE, 
                            font_size = 20, config = NULL){
  
  force(envir)
  if(is.null(config))
    config = config(plan = plan, targets = targets, envir = envir,
                        verbose = verbose, parallelism = parallelism, jobs = jobs,
                        packages = packages, prework = prework)
  graph = config$graph
  
  generic_color = "gray"
  import_color = "#1874cd"
  up_to_date_color = "#228b22"
  outdated_color = "#aa0000"
  in_progress_color = "#ff7221"
  missing_color = "#9a32cd"
  generic_shape = "dot"
  file_shape = "square"
  function_shape = "triangle"
  
  network_data = toVisNetworkData(graph)
  nodes = network_data$nodes
  rownames(nodes) = nodes$label
  edges = network_data$edges
  if(!nrow(nodes)) return(null_graph())
  
  targets = intersect(nodes$id, plan$target)
  imports = setdiff(nodes$id, plan$target)
  functions = Filter(x = imports, f = function(x) 
    can_get_function(x, envir = envir))
  missing = Filter(x = imports, f = function(x) missing_import(x, envir = envir))
  
  if(parallelism == "Makefile") # Imports and targets are parallelized separately.
    nodes = resolve_levels_Makefile(nodes = nodes, graph = graph, 
      imports = imports, targets = targets)
  else # Imports and targets are parallelized all together.
    nodes = resolve_levels(nodes = nodes, graph = graph)
  
  nodes$font.size = font_size
  nodes$status = "import"
  nodes$color = import_color
  nodes[missing, "status"] = "missing"
  nodes[missing, "color"] = missing_color
  
  outdated = outdated(plan = plan, targets = targets, envir = envir, 
     verbose = verbose, jobs = jobs, parallelism = parallelism,
     packages = packages, prework = prework, config = config)
  nodes[targets, "status"] = "up-to-date"
  nodes[targets, "color"] = up_to_date_color
  nodes[outdated, "status"] = "outdated"
  nodes[outdated, "color"] = outdated_color
  
  in_prog = in_progress()
  nodes[in_prog, "status"] = "in progress"
  nodes[in_prog, "color"] = in_progress_color

  if(build_times) 
    nodes = append_build_times(nodes, cache = config$cache)
    
  nodes$shape = generic_shape
  nodes[is_file(nodes$id), "shape"] = file_shape
  nodes[functions, "shape"] = function_shape
  
  nodes$hover_label = nodes$id
  rownames(plan) = plan$target
  nodes[targets, "hover_label"] = wraptext(plan[targets, "command"])
  
  if(nrow(edges)) edges$arrows = "to"
  
  legend_nodes = data.frame(
    label = c("Up to date", "In progress", "Outdated", "Imported", "Missing", 
      "Object", "Function", "File"),
    color = c(up_to_date_color, in_progress_color, outdated_color, import_color, 
      missing_color, generic_color, generic_color, generic_color),
    shape = c(generic_shape, generic_shape, generic_shape, generic_shape, generic_shape, 
      generic_shape, function_shape, file_shape),
    font.color = "black",
    font.size = font_size)
  legend_nodes$id = seq_len(nrow(legend_nodes))
  
  if(targets_only){
    nodes = nodes[targets,]
    edges = edges[edges$from %in% targets & edges$to %in% targets,]
  }
  list(nodes = nodes, edges = edges, legend_nodes = legend_nodes, 
    parallelism = parallelism) 
}

append_build_times = function(nodes, cache){
  x = build_times(cache = cache)
  bt = as.character(x$user + x$system)
  names(bt) = x$target
  timed = intersect(x$target, nodes$id)
  if(!length(timed)) return(nodes)
  nodes[timed, "label"] = paste(nodes[timed, "label"],
    bt, sep = "\n")
  nodes
}

can_get_function = function(x, envir){
  tryCatch({
    is.function(eval(parse(text = x), envir = envir))
  }, error = function(e) FALSE)
}

null_graph = function(){
  nodes = data.frame(id = 1, label = "Nothing to plot.")
  visNetwork(nodes = nodes, edges = data.frame(from = NA, to = NA))
}

missing_import = function(x, envir){
  missing_object = !is_file(x) & 
    is.null(envir[[x]]) & 
    tryCatch({tmp = flexible_get(x); FALSE}, error = function(e) TRUE)
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

resolve_levels_Makefile = function(nodes, graph, imports, targets){
  graph_imports = delete_vertices(graph, v = targets)
  graph_targets = delete_vertices(graph, v = imports)
  nodes_imports = nodes[nodes$id %in% imports,]
  nodes_targets = nodes[nodes$id %in% targets,]
  nodes_imports = resolve_levels(nodes = nodes_imports, graph = graph_imports)
  nodes_targets = resolve_levels(nodes = nodes_targets, graph = graph_targets)
  nodes_imports$level = nodes_imports$level - max(nodes_imports$level)
  rbind(nodes_imports, nodes_targets)
}
