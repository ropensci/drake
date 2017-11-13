#' @title Function \code{dataframes_graph}
#' @description Get the information about nodes, edges, and the legend/key
#' so you can plot your own custom \code{visNetwork}.
#' @export
#' @return A list of three data frames: one for nodes,
#' one for edges, and one for
#' the legend nodes. The list also contains the
#' default title of the graph.
#' @seealso \code{\link{vis_drake_graph}}, \code{\link{build_drake_graph}}
#' @param plan workflow plan data frame, same as for function
#' \code{\link{make}()}.
#'
#' @param targets names of targets to build, same as for function
#' \code{\link{make}()}.
#'
#' @param from Optional collection of target/import names.
#' If \code{from} is nonempty,
#' the graph will restrict itself to
#' a neighborhood of \code{from}.
#' Control the neighborhood with
#' \code{mode} and \code{order}.
#'
#' @param mode Which direction to branch out in the graph
#' to create a neighborhood around \code{from}.
#' Use \code{"in"} to go upstream,
#' \code{"out"} to go downstream,
#' and \code{"all"} to go both ways and disregard
#' edge direction altogether.
#'
#' @param order How far to branch out to create
#' a neighborhood around \code{from} (measured
#' in the number of nodes). Defaults to
#' as far as possible.
#'
#' @param subset Optional character vector of of target/import names.
#' Subset of nodes to display in the graph.
#' Applied after \code{from}, \code{mode}, and \code{order}.
#' Be advised: edges are only kept for adjacent nodes in \code{subset}.
#' If you do not select all the intermediate nodes,
#' edges will drop from the graph.
#'
#' @param envir environment to import from, same as for function
#' \code{\link{make}()}. \code{config$envir} is ignored in favor
#' of \code{envir}.
#'
#' @param verbose logical, whether to output messages to the console.
#'
#' @param hook same as for \code{\link{make}}
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
#'
#' @param packages same as for \code{\link{make}}
#'
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
#' @param make_imports logical, whether to import external files
#' and objects from the user's workspace to determine
#' which targets are up to date. If \code{FALSE}, the computation
#' is faster, but all the relevant information is drawn from the cache
#' and may be out of date.
#'
#' @param from_scratch logical, whether to assume all the targets
#' will be made from scratch on the next \code{\link{make}()}.
#' Makes all targets outdated, but keeps information about
#' build progress in previous \code{\link{make}()}s.
#'
#' @examples
#' \dontrun{
#' load_basic_example() # Load drake's canonical example.
#' # Get a list of data frames representing the nodes, edges,
#' # and legend nodes of the visNetwork graph from vis_drake_graph().
#' raw_graph <- dataframes_graph(my_plan)
#' # Choose a subset of the graph.
#' smaller_raw_graph <- dataframes_graph(
#'   my_plan,
#'   from = c("small", "reg2"),
#'   mode = "in"
#' )
#' # Inspect the raw graph.
#' str(raw_graph)
#' # Use the data frames to plot your own custom visNetwork graph.
#' # For example, you can omit the legend nodes
#' # and change the direction of the graph.
#' library(magrittr)
#' library(visNetwork)
#' visNetwork(nodes = raw_graph$nodes, edges = raw_graph$edges) %>%
#'   visHierarchicalLayout(direction = 'UD')
#' }
dataframes_graph <- function(
  plan = workplan(), targets = drake::possible_targets(plan),
  envir = parent.frame(), verbose = TRUE,
  hook = default_hook,
  cache = drake::get_cache(verbose = verbose), jobs = 1,
  parallelism = drake::default_parallelism(), packages = rev(.packages()),
  prework = character(0), build_times = TRUE, digits = 3,
  targets_only = FALSE,
  split_columns = FALSE, font_size = 20, config = NULL,
  from = NULL, mode = c("out", "in", "all"), order = NULL, subset = NULL,
  make_imports = TRUE,
  from_scratch = FALSE
) {
  force(envir)
  if (is.null(config)){
    config <- drake_config(plan = plan, targets = targets,
      envir = envir, verbose = verbose,
      hook = hook, cache = cache,
      parallelism = parallelism, jobs = jobs,
      packages = packages, prework = prework)
  }
  if (!length(V(config$graph)$name)){
    return(null_graph())
  }

  config$plan <- sanitize_plan(plan = config$plan)
  config$targets <- sanitize_targets(
    plan = config$plan, targets = config$targets)
  if (from_scratch){
    config$outdated <- config$plan$target
  } else {
    config$outdated <- outdated(config = config, make_imports = make_imports)
  }

  network_data <- visNetwork::toVisNetworkData(config$graph)
  config$nodes <- network_data$nodes
  rownames(config$nodes) <- config$nodes$label

  config$edges <- network_data$edges
  if (nrow(config$edges)){
    config$edges$arrows <- "to"
  }

  config$imports <- setdiff(config$nodes$id, config$plan$target)
  config$in_progress <- in_progress(cache = config$cache)
  config$failed <- failed(cache = config$cache)
  config$files <- Filter(x = config$nodes$id, f = is_file)
  config$functions <- Filter(x = config$imports,
    f = function(x) can_get_function(x, envir = config$envir))
  config$missing <- Filter(x = config$imports,
    f = function(x) missing_import(x, envir = config$envir))
  config$font_size <- font_size
  config$build_times <- build_times
  config$digits <- digits

  config$nodes <- configure_nodes(config = config)
  config$from <- from
  config$mode <- match.arg(mode)
  config$order <- order
  config <- trim_graph(config)
  config <- subset_nodes_edges(
    config = config,
    keep = V(config$graph)$name
  )

  if (targets_only) {
    config <- subset_nodes_edges(
      config = config,
      keep = intersect(targets, config$nodes$id)
    )
  }
  if (split_columns){
    config$nodes <- split_node_columns(nodes = config$nodes)
  }
  if (length(subset)){
    config <- subset_nodes_edges(
      config = config,
      keep = subset
    )
  }
  config$nodes <- shrink_levels(config$nodes)

  list(nodes = config$nodes, edges = config$edges,
    legend_nodes = legend_nodes(font_size = font_size),
    default_title = default_graph_title(
      parallelism = config$parallelism, split_columns = split_columns))
}
