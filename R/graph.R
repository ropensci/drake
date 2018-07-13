#' @title Create the `igraph` dependency network of your project.
#' @description This function returns an igraph object representing how
#' the targets in your workflow plan data frame
#' depend on each other.
#' (`help(package = "igraph")`). To plot this graph, call
#' to [plot.igraph()] on your graph. See the online manual
#' for enhanced graph visualization functionality.
#' @export
#' @return An igraph object representing
#'   the workflow plan dependency network.
#' @inheritParams drake_config
#' @param sanitize_plan logical, deprecated. If you must,
#'   call `drake:::sanitize_plan()` to sanitize the plan
#'   and/or `drake:::sanitize_targets()` to sanitize the targets
#'   (or just get `plan` and `targets` and `graph` from
#'   [drake_config()]).
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # Make the igraph network connecting all the targets and imports.
#' g <- build_drake_graph(my_plan)
#' class(g) # "igraph"
#' })
#' }
build_drake_graph <- function(
  plan = read_drake_plan(),
  targets = plan$target,
  envir = parent.frame(),
  verbose = drake::default_verbose(),
  jobs = 1,
  sanitize_plan = FALSE,
  console_log_file = NULL
){
  force(envir)
  if (sanitize_plan){
    warning(
      "The `sanitize_plan` argument to `build_drake_graph()` is deprecated.",
      call. = FALSE
    )
  }
  imports <- as.list(envir)
  unload_conflicts(
    imports = names(imports),
    targets = plan$target,
    envir = envir,
    verbose = verbose
  )
  import_names <- setdiff(names(imports), targets)
  imports <- imports[import_names]
  config <- list(verbose = verbose, console_log_file = console_log_file)
  console_many_targets(
    targets = names(imports),
    pattern = "connect",
    type = "import",
    config = config
  )
  imports_edges <- lightly_parallelize(
    X = seq_along(imports),
    FUN = function(i){
      imports_edges(name = import_names[[i]], value = imports[[i]])
    },
    jobs = jobs
  ) %>%
    do.call(what = dplyr::bind_rows)
  console_many_targets(
    targets = plan$target,
    pattern = "connect",
    type = "target",
    config = config
  )
  commands_deps <- lightly_parallelize(
    X = seq_len(nrow(plan)),
    FUN = function(i){
      command_dependencies(command = plan$command[i])
    },
    jobs = jobs
  )
  commands_edges <- lightly_parallelize(
    X = seq_len(nrow(plan)),
    FUN = function(i){
      code_deps_to_edges(target = plan$target[i], deps = commands_deps[[i]])
    },
    jobs = jobs
  ) %>%
    do.call(what = dplyr::bind_rows)
  file_outs <- lightly_parallelize(
    X = seq_len(nrow(plan)),
    FUN = function(i){
      commands_deps[[i]]$file_out
    },
    jobs = jobs
  ) %>%
    setNames(nm = plan$target) %>%
    select_nonempty
  commands_edges <- connect_file_outs(commands_edges, file_outs)
  graph <- dplyr::bind_rows(imports_edges, commands_edges) %>%
    igraph::graph_from_data_frame()
  if (length(file_outs)){
    graph <- igraph::set_vertex_attr(
      graph = graph,
      name = "output_files",
      index = names(file_outs),
      value = file_outs
    )
  }
  prune_drake_graph(graph = graph, to = targets, jobs = jobs) %>%
    igraph::simplify(remove.multiple = TRUE, remove.loops = TRUE)
}

imports_edges <- function(name, value){
  deps <- import_dependencies(value)
  code_deps_to_edges(target = name, deps = deps)
}

code_deps_to_edges <- function(target, deps){
  inputs <- clean_dependency_list(deps[setdiff(names(deps), "file_out")])
  edges <- NULL
  if (length(inputs)){
    data.frame(from = inputs, to = target, stringsAsFactors = FALSE)
  } else {
    # Loops will be removed.
    data.frame(from = target, to = target, stringsAsFactors = FALSE)
  }
}

#' @title Prune the dependency network of your project.
#' @export
#' @seealso [build_drake_graph()], [drake_config()],
#'   [make()]
#' @description `igraph` objects are used
#' internally to represent the dependency network of your workflow.
#' See `drake_config(my_plan)$graph` from the mtcars example.
#' @details For a supplied graph, take the subgraph of all combined
#' incoming paths to the vertices in `to`. In other words,
#' remove the vertices after `to` from the graph.
#' @return A pruned igraph object representing the dependency network
#'   of the workflow.
#' @param graph An igraph object to be pruned.
#' @param to Character vector, names of the vertices that draw
#'   the line for pruning. The pruning process removes all vertices
#'   downstream of `to`.
#' @param jobs Number of jobs for light parallelism (on non-Windows machines).
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # Build the igraph object representing the workflow dependency network.
#' # You could also use drake_config(my_plan)$graph
#' graph <- build_drake_graph(my_plan)
#' # The default plotting is not the greatest,
#' # but you will get the idea.
#' # plot(graph) # nolint
#' # Prune the graph: that is, remove the nodes downstream
#' # from 'small' and 'large'
#' pruned <- prune_drake_graph(graph = graph, to = c("small", "large"))
#' # plot(pruned) # nolint
#' })
#' }
prune_drake_graph <- function(
  graph, to = igraph::V(graph)$name, jobs = 1
){
  if (!inherits(graph, "igraph")){
    stop(
      "supplied graph must be an igraph object",
      call. = FALSE
    )
  }
  unlisted <- setdiff(to, V(graph)$name)
  if (length(unlisted)){
    warning(
      "supplied targets not in the dependency graph:\n",
      multiline_message(unlisted),
      call. = FALSE
    )
    to <- setdiff(to, unlisted)
  }
  if (!length(to)){
    warning(
      "cannot prune graph: no valid destination vertices supplied",
      call. = FALSE
    )
    return(graph)
  }
  ignore <- lightly_parallelize(
    X = to,
    FUN = function(vertex){
      subcomponent(graph = graph, v = vertex, mode = "in")$name
    },
    jobs = jobs
  ) %>%
    unlist() %>%
    unique() %>%
    setdiff(x = igraph::V(graph)$name)
  delete_vertices(graph = graph, v = ignore)
}

unload_conflicts <- function(imports, targets, envir, verbose){
  common <- intersect(imports, targets)
  if (verbose & length(common)){
    message(
      "Unloading targets from environment:\n",
      multiline_message(common), sep = ""
    )
  }
  remove(list = common, envir = envir)
}

get_neighborhood <- function(graph, from, mode, order){
  if (!length(order)){
    order <- length(V(graph))
  }
  if (length(from)){
    from <- sanitize_nodes(nodes = from, choices = V(graph)$name)
    graph <- igraph::make_ego_graph(
      graph = graph,
      order = order,
      nodes = from,
      mode = mode
    ) %>%
      do.call(what = igraph::union)
  }
  graph
}

downstream_nodes <- function(from, graph, jobs){
  if (!length(from)){
    return(character(0))
  }
  lightly_parallelize(
    X = from,
    FUN = function(node){
      subcomponent(graph, v = node, mode = "out")$name
    },
    jobs = jobs
  ) %>%
    unlist() %>%
    unique() %>%
    sort()
}

leaf_nodes <- function(graph){
  is_leaf <- igraph::degree(graph, mode = "in") == 0
  V(graph)[is_leaf]$name
}

filter_upstream <- function(targets, graph){
  delete_these <- setdiff(V(graph)$name, targets)
  graph <- delete_vertices(graph = graph, v = delete_these)
  leaf_nodes(graph)
}

subset_graph <- function(graph, subset){
  if (!length(subset)){
    return(graph)
  }
  subset <- intersect(subset, V(graph)$name)
  igraph::induced_subgraph(graph = graph, vids = subset)
}

imports_graph <- function(config){
  delete_these <- intersect(config$plan$target, V(config$graph)$name)
  delete_vertices(config$graph, v = delete_these)
}

targets_graph <- function(config){
  delete_these <- setdiff(V(config$graph)$name, config$plan$target)
  delete_vertices(config$graph, v = delete_these)
}

connect_file_outs <- function(commands_edges, file_outs){
  if (!length(file_outs)){
    return(commands_edges)
  }
  file_outs <- utils::stack(file_outs)
  file_outs$ind <- as.character(file_outs$ind)
  index <- match(commands_edges$from, table = file_outs$values)
  commands_edges$from[is.finite(index)] <- file_outs$ind[na.omit(index)]
  commands_edges
}
