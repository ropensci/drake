#' @title Create the underlying node and edge data frames
#' behind \code{\link{vis_drake_graph}()}.
#' @description With the returned data frames,
#' you can plot your own custom \code{visNetwork} graph.
#' @export
#' @return A list of three data frames: one for nodes,
#' one for edges, and one for
#' the legend nodes. The list also contains the
#' default title of the graph.
#' @seealso \code{\link{vis_drake_graph}}, \code{\link{build_drake_graph}}
#' @param config a \code{\link{drake_config}()} configuration list.
#' You can get one as a return value from \code{\link{make}()} as well.
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
#' edges will drop from the graph.=
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
#' @param font_size numeric, font size of the node labels in the graph
#'
#' @param build_times logical, whether to show the \code{\link{build_times}()}
#' of the targets and imports, if available.
#' These are just elapsed times from \code{system.time()}.
#'
#' @param digits number of digits for rounding the build times
#'
#' @param from_scratch logical, whether to assume all the targets
#' will be made from scratch on the next \code{\link{make}()}.
#' Makes all targets outdated, but keeps information about
#' build progress in previous \code{\link{make}()}s.
#'
#' @param make_imports logical, whether to make the imports first.
#' Set to \code{FALSE} to increase speed and risk using obsolete information.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' config <- load_basic_example() # Load drake's canonical example.
#' # Get a list of data frames representing the nodes, edges,
#' # and legend nodes of the visNetwork graph from vis_drake_graph().
#' raw_graph <- dataframes_graph(config = config)
#' # Choose a subset of the graph.
#' smaller_raw_graph <- dataframes_graph(
#'   config = config,
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
#' })
#' }
dataframes_graph <- function(
  config,
  from = NULL,
  mode = c("out", "in", "all"),
  order = NULL,
  subset = NULL,
  build_times = TRUE,
  digits = 3,
  targets_only = FALSE,
  split_columns = FALSE,
  font_size = 20,
  from_scratch = FALSE,
  make_imports = TRUE
) {
  if (!length(V(config$graph)$name)){
    return(null_graph())
  }
  config$build_times <- build_times
  config$digits <- digits
  config$font_size <- font_size
  config$from_scratch <- from_scratch
  config$make_imports <- make_imports
  config$split_columns <- split_columns
  config <- get_raw_node_category_data(config)
  config$stages <- graphing_parallel_stages(config)

  config$graph <- get_neighborhood(
    graph = config$graph,
    from = from,
    mode = match.arg(mode),
    order = order
  )
  config$graph <- subset_graph(graph = config$graph, subset = subset)
  if (targets_only){
    config$graph <- subset_graph(
      graph = config$graph,
      subset = config$plan$target
    )
  }

  network_data <- visNetwork::toVisNetworkData(config$graph)
  config$nodes <- network_data$nodes
  config <- trim_node_categories(config)
  config$nodes <- configure_nodes(config = config)

  config$edges <- network_data$edges
  if (nrow(config$edges)){
    config$edges$arrows <- "to"
  }

  list(nodes = config$nodes, edges = config$edges,
    legend_nodes = legend_nodes(font_size = font_size),
    default_title = default_graph_title(split_columns = split_columns))
}
