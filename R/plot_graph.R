#' @title Show an interactive visual network representation
#'   of your drake project.
#' @description To save time for repeated plotting,
#' this function is divided into
#' [dataframes_graph()] and [render_drake_graph()].
#' @export
#' @aliases drake_graph
#' @seealso [build_drake_graph()]
#' @return A visNetwork graph.
#'
#' @param from Optional character vector of target/import names.
#'   If `from` is nonempty,
#'   the graph will restrict itself to
#'   a neighborhood of `from`.
#'   Control the neighborhood with
#'   `mode` and `order`.
#'
#' @param mode Which direction to branch out in the graph
#'   to create a neighborhood around `from`.
#'   Use `"in"` to go upstream,
#'   `"out"` to go downstream,
#'   and `"all"` to go both ways and disregard
#'   edge direction altogether.
#'
#' @param order How far to branch out to create
#'   a neighborhood around `from` (measured
#'   in the number of nodes). Defaults to
#'   as far as possible.
#'
#' @param subset Optional character vector of of target/import names.
#'   Subset of nodes to display in the graph.
#'   Applied after `from`, `mode`, and `order`.
#'   Be advised: edges are only kept for adjacent nodes in `subset`.
#'   If you do not select all the intermediate nodes,
#'   edges will drop from the graph.
#'
#' @param file Name of HTML file to save the graph.
#'   If `NULL` or `character(0)`,
#'   no file is saved and the graph is rendered and displayed within R.
#'
#' @param selfcontained logical, whether to save
#'   the `file` as a self-contained
#'   HTML file (with external resources base64 encoded) or a file with
#'   external resources placed in an adjacent directory. If `TRUE`,
#'   pandoc is required.
#'
#' @param build_times logical, whether to print the [build_times()]
#'   in the graph.
#'
#' @param digits number of digits for rounding the build times
#'
#' @param targets_only logical, whether to skip the imports and only show the
#'   targets in the workflow plan.
#'
#' @param split_columns logical, whether to break up the
#'   columns of nodes to make the aspect ratio of the rendered
#'   graph closer to 1:1. This improves the viewing experience,
#'   but the columns no longer strictly represent parallelizable
#'   stages of build items. (Although the targets/imports
#'   in each column are still conditionally independent,
#'   there may be more conditional independence than the graph
#'   indicates.)
#'
#' @param font_size numeric, font size of the node labels in the graph
#'
#' @param layout name of an igraph layout
#'   to use, such as 'layout_with_sugiyama'
#'   or 'layout_as_tree'. Be careful with
#'   'layout_as_tree': the graph is a directed
#'   acyclic graph, but not necessarily a tree.
#'
#' @param direction an argument to
#'   [visNetwork::visHierarchicalLayout()]
#'   indicating the direction of the graph.
#'   Options include 'LR', 'RL', 'DU', and 'UD'.
#'   At the time of writing this, the letters must be capitalized,
#'   but this may not always be the case ;) in the future.
#'
#' @param navigationButtons logical, whether to add navigation buttons with
#'   `visNetwork::visInteraction(navigationButtons = TRUE)`
#'
#' @param hover logical, whether to show the command that generated the target
#'   when you hover over a node with the mouse. For imports, the label does not
#'   change with hovering.
#'
#' @param main title of the graph
#'
#' @param ncol_legend number of columns in the legend nodes
#'
#' @param full_legend logical. If `TRUE`, all the node types
#'   are printed in the legend. If `FALSE`, only the
#'   node types used are printed in the legend.
#'
#' @param make_imports logical, whether to import external files
#'   and objects from the user's workspace to determine
#'   which targets are up to date. If `FALSE`, the computation
#'   is faster, but all the relevant information is drawn from the cache
#'   and may be out of date.
#'
#' @param from_scratch logical, whether to assume all the targets
#'   will be made from scratch on the next [make()].
#'   Makes all targets outdated, but keeps information about
#'   build progress in previous [make()]s.
#'
#' @param ... other arguments passed to
#'   [visNetwork::visNetwork()] to plot the graph.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' config <- load_basic_example() # Get the code with drake_example("basic").
#' # Plot the network graph representation of the workflow.
#' vis_drake_graph(config, width = '100%') # The width is passed to visNetwork
#' config <- make(my_plan) # Run the project, build the targets.
#' vis_drake_graph(config) # The red nodes from before are now green.
#' # Plot a subgraph of the workflow.
#' vis_drake_graph(
#'   config,
#'   from = c("small", "reg2"),
#'   to = "summ_regression2_small"
#' )
#' })
#' }
vis_drake_graph <- function(
  config,
  file = character(0), selfcontained = FALSE,
  build_times = TRUE, digits = 3, targets_only = FALSE,
  split_columns = FALSE, font_size = 20,
  layout = "layout_with_sugiyama", main = NULL,
  direction = "LR", hover = TRUE,
  navigationButtons = TRUE, # nolint
  from = NULL, mode = c("out", "in", "all"), order = NULL,
  subset = NULL,
  ncol_legend = 1,
  full_legend = TRUE,
  make_imports = TRUE,
  from_scratch = FALSE,
  ...
){
  raw_graph <- dataframes_graph(
    config = config,
    from = from,
    mode = mode,
    order = order,
    subset = subset,
    build_times = build_times,
    digits = digits,
    targets_only = targets_only,
    split_columns = split_columns,
    font_size = font_size,
    make_imports = make_imports,
    from_scratch = from_scratch
  )
  if (is.null(main)){
    main <- raw_graph$default_title
  }
  render_drake_graph(raw_graph, file = file, selfcontained = selfcontained,
    layout = layout, direction = direction,
    navigationButtons = navigationButtons, # nolint
    hover = hover, main = main,
    ncol_legend = ncol_legend, full_legend = full_legend, ...)
}

#' @export
drake_graph <- vis_drake_graph

#' @title Render a visualization using the data frames
#'   generated by [dataframes_graph()].
#' @description This function is called inside
#' [vis_drake_graph()], which typical users
#' call more often.
#' @export
#' @return A visNetwork graph.
#'
#' @param graph_dataframes list of data frames generated by
#'   [dataframes_graph()].
#'   There should be 3 data frames: `nodes`, `edges`,
#'   and `legend_nodes`.
#'
#' @param file Name of HTML file to save the graph.
#'   If `NULL` or `character(0)`,
#'   no file is saved and the graph is rendered and displayed within R.
#'
#' @param layout name of an igraph layout to use,
#'   such as 'layout_with_sugiyama'
#'   or 'layout_as_tree'.
#'   Be careful with 'layout_as_tree': the graph is a directed
#'   acyclic graph, but not necessarily a tree.
#'
#' @param selfcontained logical, whether
#'   to save the `file` as a self-contained
#'   HTML file (with external resources base64 encoded) or a file with
#'   external resources placed in an adjacent directory. If `TRUE`,
#'   pandoc is required.
#'
#' @param direction an argument to [visNetwork::visHierarchicalLayout()]
#'   indicating the direction of the graph.
#'   Options include 'LR', 'RL', 'DU', and 'UD'.
#'   At the time of writing this, the letters must be capitalized,
#'   but this may not always be the case ;) in the future.
#'
#' @param navigationButtons logical, whether to add navigation buttons with
#'   `visNetwork::visInteraction(navigationButtons = TRUE)`
#'
#' @param hover logical, whether to show the command that generated the target
#'   when you hover over a node with the mouse. For imports, the label does not
#'   change with hovering.
#'
#' @param main title of the graph
#'
#' @param ncol_legend number of columns in the legend nodes.
#'   To remove the legend entirely, set `ncol_legend` to `NULL` or `0`.
#'   
#' @param full_legend logical. If `TRUE`, all the node types
#'   are printed in the legend. If `FALSE`, only the
#'   node types used are printed in the legend.
#'
#' @param ... arguments passed to [visNetwork()].
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' # Instead of jumpting right to vis_drake_graph(), get the data frames
#' # of nodes, edges, and legend nodes.
#' config <- drake_config(my_plan) # Internal configuration list
#' graph <- dataframes_graph(config)
#' # You can pass the data frames right to render_drake_graph()
#' # (as in vis_drake_graph()) or you can create
#' # your own custom visNewtork graph.
#' render_drake_graph(graph, width = '100%') # Width is passed to visNetwork.
#' })
#' }
render_drake_graph <- function(
  graph_dataframes, file = character(0),
  layout = "layout_with_sugiyama", direction = "LR", hover = TRUE,
  main = graph_dataframes$default_title, selfcontained = FALSE,
  navigationButtons = TRUE, # nolint
  ncol_legend = 1, full_legend = TRUE,
  ...
){
  if (!full_legend){
    graph_dataframes$legend_nodes <- filter_legend_nodes(graph_dataframes)
  }
  out <- visNetwork::visNetwork(
    nodes = graph_dataframes$nodes,
    edges = graph_dataframes$edges,
    main = main, ...
  ) %>%
    visNetwork::visHierarchicalLayout(direction = direction)
  if (length(ncol_legend) && ncol_legend > 0){
    out <- visNetwork::visLegend(
      graph = out,
      useGroups = FALSE,
      addNodes = graph_dataframes$legend_nodes,
      ncol = ncol_legend
    )
  }
  if (nrow(graph_dataframes$edges)){
    out <- visNetwork::visIgraphLayout(
      graph = out,
      physics = FALSE,
      randomSeed = 2017,
      layout = layout
    )
  }
  if (navigationButtons) # nolint
    out <- visNetwork::visInteraction(out,
      navigationButtons = TRUE) # nolint
  if (hover)
    out <- with_hover(out)
  if (length(file)) {
    visNetwork::visSave(graph = out, file = file,
      selfcontained = selfcontained)
    return(invisible())
  }
  out
}

with_hover <- function(x) {
  visNetwork::visInteraction(x, hover = TRUE) %>%
    visNetwork::visEvents(hoverNode =
      "function(e){
        var label_info = this.body.data.nodes.get({
          fields: ['label', 'hover_label'],
          filter: function (item) {
            return item.id === e.node
          },
          returnType :'Array'
        });
        this.body.data.nodes.update({
          id: e.node,
          label : label_info[0].hover_label,
          hover_label : label_info[0].label
        });
      }"
    ) %>%
    visNetwork::visEvents(blurNode =
      "function(e){
        var label_info = this.body.data.nodes.get({
          fields: ['label', 'hover_label'],
          filter: function (item) {
            return item.id === e.node
          },
          returnType :'Array'
        });
        this.body.data.nodes.update({
          id: e.node,
          label : label_info[0].hover_label,
          hover_label : label_info[0].label
        });
      }"
    )
}

filter_legend_nodes <- function(dfs){
  colors <- unique(dfs$nodes$color)
  shapes <- unique(dfs$nodes$shape)
  ln <- dfs$legend_nodes
  ln[ln$color %in% colors & ln$shape %in% shapes, , drop = FALSE] # nolint
}
