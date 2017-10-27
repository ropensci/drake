#' @title Function \code{plot_graph}
#' @description Plot the dependency structure of your workplan.
#' IMPORTANT: you must be in the root directory of your project.
#' To save time for repeated plotting, this function is divided into
#' \code{\link{dataframes_graph}()} and \code{\link{render_graph}()}.
#' @export
#' @seealso \code{\link{build_graph}}
#'
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
#' @param hook same as for \code{\link{make}}
#'
#' @param cache optional drake cache. See code{\link{new_cache}()}. If
#' The \code{cache} argument is ignored if a non-null \code{config}
#' argument is supplied.
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
#' \code{parallelism} equals \code{'Makefile'} because in that case,
#' all the imports are imported before any target is built.
#' Thus, the arrangement in the graph is different for Makefile parallelism.
#' See \code{?parallelism_choices} for details.
#'
#' @param packages same as for \code{\link{make}()}.
#'
#' @param prework same as for \code{\link{make}()}.
#'
#' @param file Name of HTML file to save the graph.
#' If \code{NULL} or \code{character(0)},
#' no file is saved and the graph is rendered and displayed within R.
#'
#' @param selfcontained logical, whether to save
#' the \code{file} as a self-contained
#' HTML file (with external resources base64 encoded) or a file with
#' external resources placed in an adjacent directory. If \code{TRUE},
#' pandoc is required.
#'
#' @param build_times logical, whether to print the \code{\link{build_times}()}
#' in the graph.
#'
#' @param digits number of digits for rounding the build times
#'
#' @param targets_only logical, whether to skip the imports and only show the
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
#' @param layout name of an igraph layout
#' to use, such as 'layout_with_sugiyama'
#' or 'layout_as_tree'. Be careful with
#' 'layout_as_tree': the graph is a directed
#' acyclic graph, but not necessarily a tree.
#'
#' @param direction an argument to
#' \code{visNetwork::visHierarchicalLayout()}
#' indicating the direction of the graph.
#' Options include 'LR', 'RL', 'DU', and 'UD'.
#' At the time of writing this, the letters must be capitalized,
#' but this may not always be the case ;) in the future.
#'
#' @param navigationButtons logical, whether to add navigation buttons with
#' \code{visNetwork::visInteraction(navigationButtons = TRUE)}
#'
#' @param hover logical, whether to show the command that generated the target
#' when you hover over a node with the mouse. For imports, the label does not
#' change with hovering.
#'
#' @param config option internal runtime parameter list of
#' \code{\link{make}(...)},
#' produced with \code{\link{config}()}.
#' \code{Config} overrides all arguments except
#' \code{file}, \code{selfcontained}, \code{build_times},
#' \code{digits}, \code{targets_only}, \code{split_columns},
#' \code{font_size}, \code{layout}, \code{main}, \code{direction},
#' \code{hover}, \code{navigationButtons}, and \code{...}.
#' Computing \code{config}
#' in advance could save time if you plan multiple calls to
#' \code{outdated()}.
#'
#' @param main title of the graph
#'
#' @param ... other arguments passed to
#' \code{visNetwork::visNetwork()} to plot the graph.
#'
#' @examples
#' \dontrun{
#' load_basic_example()
#' plot_graph(my_plan, width = '100%') # The width is passed to visNetwork
#' make(my_plan)
#' plot_graph(my_plan) # The red nodes from before are now green.
#' }
plot_graph <- function(
  plan = workplan(), targets = drake::possible_targets(plan),
  envir = parent.frame(), verbose = TRUE,
  hook = function(code){
    force(code)
  },
  cache = drake::get_cache(),
  jobs = 1, parallelism = drake::default_parallelism(),
  packages = (.packages()), prework = character(0),
  config = NULL,
  file = character(0), selfcontained = FALSE,
  build_times = TRUE, digits = 3, targets_only = FALSE,
  split_columns = FALSE, font_size = 20,
  layout = "layout_with_sugiyama", main = NULL,
  direction = "LR", hover = TRUE,
  navigationButtons = TRUE, # nolint
  ...
){
  force(envir)
  raw_graph <- dataframes_graph(plan = plan, targets = targets,
    envir = envir, verbose = verbose, hook = hook, cache = cache,
    jobs = jobs, parallelism = parallelism,
    packages = packages, prework = prework,
    build_times = build_times, digits = digits,
    targets_only = targets_only, split_columns = split_columns,
    config = config, font_size = font_size)
  if (is.null(main)){
    main <- raw_graph$default_title
  }
  render_graph(raw_graph, file = file, selfcontained = selfcontained,
    layout = layout, direction = direction,
    navigationButtons = navigationButtons, # nolint
    hover = hover, main = main, ...)
}

#' @title Function \code{render_graph}
#' @description render a graph from the data frames
#' generated by \code{\link{dataframes_graph}()}
#' @export
#'
#' @param graph_dataframes list of data frames generated by
#' \code{\link{dataframes_graph}()}.
#' There should be 3 data frames: \code{nodes}, \code{edges},
#' and \code{legend_nodes}.
#'
#' @param file Name of HTML file to save the graph.
#' If \code{NULL} or \code{character(0)},
#' no file is saved and the graph is rendered and displayed within R.
#'
#' @param layout name of an igraph layout to use,
#' such as 'layout_with_sugiyama'
#' or 'layout_as_tree'.
#' Be careful with 'layout_as_tree': the graph is a directed
#' acyclic graph, but not necessarily a tree.
#'
#' @param selfcontained logical, whether
#' to save the \code{file} as a self-contained
#' HTML file (with external resources base64 encoded) or a file with
#' external resources placed in an adjacent directory. If \code{TRUE},
#' pandoc is required.
#'
#' @param direction an argument to \code{visNetwork::visHierarchicalLayout()}
#' indicating the direction of the graph.
#' Options include 'LR', 'RL', 'DU', and 'UD'.
#' At the time of writing this, the letters must be capitalized,
#' but this may not always be the case ;) in the future.
#'
#' @param navigationButtons logical, whether to add navigation buttons with
#' \code{visNetwork::visInteraction(navigationButtons = TRUE)}
#'
#' @param hover logical, whether to show the command that generated the target
#' when you hover over a node with the mouse. For imports, the label does not
#' change with hovering.
#'
#' @param main title of the graph
#'
#' @param ... arguments passed to \code{visNetwork()}.
#'
#' @examples
#' \dontrun{
#' load_basic_example()
#' graph = dataframes_graph(my_plan)
#' render_graph(graph, width = '100%') # The width is passed to visNetwork
#' }
render_graph <- function(
  graph_dataframes, file = character(0),
  layout = "layout_with_sugiyama", direction = "LR", hover = TRUE,
  main = graph_dataframes$default_title, selfcontained = FALSE,
  navigationButtons = TRUE, # nolint
  ...
){
  out <- visNetwork::visNetwork(
    nodes = graph_dataframes$nodes,
    edges = graph_dataframes$edges,
    main = main, ...
  ) %>%
    visNetwork::visLegend(
      useGroups = FALSE,
      addNodes = graph_dataframes$legend_nodes
    ) %>%
    visNetwork::visHierarchicalLayout(direction = direction) %>%
    visNetwork::visIgraphLayout(
      physics = FALSE,
      randomSeed = 2017,
      layout = layout
    )
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
  visNetwork::visInteraction(x, hover = T) %>%
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
