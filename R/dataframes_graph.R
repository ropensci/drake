#' @title Function \code{dataframes_graph}
#' @description Get the information about nodes, edges, and the legend/key
#' so you can plot your own custom \code{visNetwork}.
#' @export
#' @return a list of three data frames: one for nodes,
#' one for edges, and one for
#' the legend/key nodes. The list also contains the
#' default title of the graph.
#' @seealso \code{\link{plot_graph}}, \code{\link{build_graph}}
#' @param plan workflow plan data frame, same as for function
#' \code{\link{make}()}.
#'
#' @param targets names of targets to build, same as for function
#' \code{\link{make}()}.
#'
#' @param envir environment to import from, same as for function
#' \code{\link{make}()}. \code{config$envir} is ignored in favor
#' of \code{envir}.
#'
#' @param verbose logical, whether to output messages to the console.
#'
#' @param cache optional drake cache. Only used if the \code{config}
#' argument is \code{NULL} (default). See code{\link{new_cache}()}.
#'
#' @param jobs The \code{outdated()} function is called internally,
#' and it needs to import objects and examine your
#' input files to see what has been updated. This could take some time,
#' and parallel computing may be needed
#' to speed up the process. The \code{jobs} argument is number of parallel jobs
#' to use for faster computation.
#'
#' @param parallelism Choice of parallel backend to speed up the computation.
#' Execution order in \code{\link{make}()} is slightly different when
#' \code{parallelism} equals \code{'Makefile'}
#' because in that case, all the imports are imported
#' before any target is built.
#' Thus, the arrangement in the graph is different for Makefile parallelism.
#' See \code{?parallelism_choices} for details.
#'
#' @param font_size numeric, font size of the node labels in the graph
#' @param packages same as for \code{\link{make}}
#' @param prework same as for \code{\link{make}}
#'
#' @param build_times logical, whether to show the \code{\link{build_times}()}
#' of the targets and imports, if available.
#' These are just elapsed times from \code{system.time()}.
#'
#' @param digits number of digits for rounding the build times
#'
#' @param targets_only logical,
#' whether to skip the imports and only include the
#' targets in the workflow plan.
#'
#' @param split_columns logical, whether to break up the
#' columns of nodes to make the aspect ratio of the rendered
#' graph closer to 1:1. This improves the viewing experience,
#' but the columns no longer strictly represent parallelizable
#' stages of build items. (Although the targets/imports
#' in each column are still conditionally independent,
#' there may be more conditional independence than the graph
#' indicates.)
#'
#' @param config option internal runtime parameter list of
#' \code{\link{make}(...)},
#' produced with \code{\link{config}()}.
#' \code{config$envir} is ignored.
#' Otherwise, computing this
#' in advance could save time if you plan multiple calls to
#' \code{dataframes_graph()}.
#' If not \code{NULL},
#' \code{config} overrides all arguments except
#' \code{build_times}, \code{digits}, \code{targets_only},
#' \code{split_columns}, and \code{font_size}.
#'
#' @examples
#' \dontrun{
#' load_basic_example()
#' raw_graph <- dataframes_graph(my_plan)
#' str(raw_graph)
#' # Plot your own custom visNetwork graph
#' library(magrittr)
#' library(visNetwork)
#' visNetwork(nodes = raw_graph$nodes, edges = raw_graph$edges) %>%
#'   visLegend(useGroups = FALSE, addNodes = raw_graph$legend_nodes) %>%
#'   visHierarchicalLayout(direction = 'LR')
#' }
dataframes_graph <- function(
  plan = drake::plan(), targets = drake::possible_targets(plan),
  envir = parent.frame(), verbose = TRUE,
  cache = drake::get_cache(), jobs = 1,
  parallelism = drake::default_parallelism(), packages = (.packages()),
  prework = character(0), build_times = TRUE, digits = 3,
  targets_only = FALSE,
  split_columns = FALSE, font_size = 20, config = NULL) {
  force(envir)
  if (is.null(config)){
    config <- config(plan = plan, targets = targets,
      envir = envir, verbose = verbose, cache = cache,
      parallelism = parallelism, jobs = jobs,
      packages = packages, prework = prework)
  }
  network_data <- visNetwork::toVisNetworkData(config$graph)
  config$nodes <- network_data$nodes
  rownames(config$nodes) <- config$nodes$label
  if (!nrow(config$nodes)){
    return(null_graph())
  }

  config$imports <- setdiff(config$nodes$id, config$plan$target)
  config$in_progress <- in_progress()
  config$outdated <- outdated(config = config)
  config$files <- Filter(x = config$nodes$id, f = is_file)
  config$functions <- Filter(x = config$imports,
    f = function(x) can_get_function(x, envir = envir))
  config$missing <- Filter(x = config$imports,
    f = function(x) missing_import(x, envir = envir))
  config$font_size <- font_size
  config$build_times <- build_times
  config$digits <- digits

  nodes <- configure_nodes(config = config)
  edges <- network_data$edges
  if (nrow(edges))
    edges$arrows <- "to"
  if (targets_only) {
    nodes <- nodes[targets, ]
    edges <-
      edges[edges$from %in% targets & edges$to %in% targets, ]
  }

  # Cannot split columns until imports are removed,
  # if applicable.
  if (split_columns){
    nodes <- split_node_columns(nodes = nodes)
  }

  list(nodes = nodes, edges = edges,
    legend_nodes = legend_nodes(font_size = font_size),
    default_title = default_graph_title(
      parallelism = config$parallelism, split_columns = split_columns))
}
