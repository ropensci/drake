#' @title Prepare the workflow graph for visualization
#' `r lifecycle::badge("stable")`
#' @description With the returned data frames,
#' you can plot your own custom `visNetwork` graph.
#' @export
#' @return A list of three data frames: one for nodes,
#'   one for edges, and one for
#'   the legend nodes. The list also contains the
#'   default title of the graph.
#' @seealso [vis_drake_graph()]
#' @param ... Arguments to [make()], such as `plan` and `targets`.
#'
#' @param config Deprecated.
#'
#' @param from Optional collection of target/import names.
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
#'   a neighborhood around `from`. Defaults to
#'   as far as possible. If a target is in the neighborhood, then
#'   so are all of its custom [file_out()] files if
#'   `show_output_files` is `TRUE`.
#'   That means the actual graph order may be slightly greater than
#'   you might expect, but this ensures consistency
#'   between `show_output_files = TRUE` and
#'   `show_output_files = FALSE`.
#'
#' @param subset Optional character vector.
#'   Subset of targets/imports to display in the graph.
#'   Applied after `from`, `mode`, and `order`.
#'   Be advised: edges are only kept for adjacent nodes in `subset`.
#'   If you do not select all the intermediate nodes,
#'   edges will drop from the graph.
#'
#' @param targets_only Logical,
#'   whether to skip the imports and only include the
#'   targets in the workflow plan.
#'
#' @param font_size Numeric, font size of the node labels in the graph
#'
#' @param build_times Character string or logical.
#'   If character, the choices are
#'     1. `"build"`: runtime of the command plus the time
#'       it take to store the target or import.
#'     2. `"command"`: just the runtime of the command.
#'     3. `"none"`: no build times.
#'   If logical, `build_times` selects whether to show the
#'   times from `build_times(..., type = "build")`` or use
#'   no build times at all. See [build_times()] for details.
#'
#' @param digits Number of digits for rounding the build times
#'
#' @param from_scratch Logical, whether to assume all the targets
#'   will be made from scratch on the next [make()].
#'   Makes all targets outdated, but keeps information about
#'   build progress in previous [make()]s.
#'
#' @param make_imports Logical, whether to make the imports first.
#'   Set to `FALSE` to increase speed and risk using obsolete information.
#'
#' @param full_legend Logical. If `TRUE`, all the node types
#'   are printed in the legend. If `FALSE`, only the
#'   node types used are printed in the legend.
#'
#' @param group Optional character scalar, name of the column used to
#'   group nodes into columns. All the columns names of your original `drake`
#'   plan are choices. The other choices (such as `"status"`) are column names
#'   in the `nodes` . To group nodes into clusters in the graph,
#'   you must also supply the `clusters` argument.
#'
#' @param clusters Optional character vector of values to cluster on.
#'   These values must be elements of the column of the `nodes` data frame
#'   that you specify in the `group` argument to `drake_graph_info()`.
#'
#' @param show_output_files Logical, whether to include
#'   [file_out()] files in the graph.
#'
#' @param hover Logical, whether to show text (file contents,
#'   commands, etc.) when you hover your cursor over a node.
#' @param on_select_col Optional string corresponding to the column name
#'   in the plan that should provide data for the `on_select` event.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (requireNamespace("visNetwork", quietly = TRUE)) {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' vis_drake_graph(my_plan)
#' # Get a list of data frames representing the nodes, edges,
#' # and legend nodes of the visNetwork graph from vis_drake_graph().
#' raw_graph <- drake_graph_info(my_plan)
#' # Choose a subset of the graph.
#' smaller_raw_graph <- drake_graph_info(
#'   my_plan,
#'   from = c("small", "reg2"),
#'   mode = "in"
#' )
#' # Inspect the raw graph.
#' str(raw_graph)
#' # Use the data frames to plot your own custom visNetwork graph.
#' # For example, you can omit the legend nodes
#' # and change the direction of the graph.
#' library(visNetwork)
#' graph <- visNetwork(nodes = raw_graph$nodes, edges = raw_graph$edges)
#' visHierarchicalLayout(graph, direction = 'UD')
#' }
#' }
#' })
#' }
drake_graph_info <- function(
  ...,
  from = NULL,
  mode = c("out", "in", "all"),
  order = NULL,
  subset = NULL,
  build_times = "build",
  digits = 3,
  targets_only = FALSE,
  font_size = 20,
  from_scratch = FALSE,
  make_imports = TRUE,
  full_legend = FALSE,
  group = NULL,
  clusters = NULL,
  show_output_files = TRUE,
  hover = FALSE,
  on_select_col = NULL,
  config = NULL
) {
}

#' @title Internal function
#' @export
#' @keywords internal
#' @description Not a user-side function.
#' @param config A [drake_config()] object.
drake_graph_info_impl <- function(
  config,
  from = NULL,
  mode = c("out", "in", "all"),
  order = NULL,
  subset = NULL,
  build_times = "build",
  digits = 3,
  targets_only = FALSE,
  font_size = 20,
  from_scratch = FALSE,
  make_imports = TRUE,
  full_legend = FALSE,
  group = NULL,
  clusters = NULL,
  show_output_files = TRUE,
  hover = FALSE,
  on_select_col = NULL
) {
  assert_pkg("visNetwork")
  assert_config(config)
  if (!length(V(config$graph)$name)) {
    return(null_graph())
  }
  config$build_times <- resolve_build_times(build_times)
  config$digits <- digits
  config$font_size <- font_size
  config$from_scratch <- from_scratch
  config$make_imports <- make_imports
  config$group <- as.character(group)
  config$clusters <- as.character(clusters)
  config$hover <- hover
  config$on_select_col <- on_select_col
  config$file_out <- lapply(all_targets(config), function(target) {
    config$spec[[target]]$deps_build$file_out
  })
  names(config$file_out) <- all_targets(config)
  if (!show_output_files) {
    vertices <- igraph::V(config$graph)$name
    imported <- igraph::V(config$graph)$imported
    vertices <- vertices[!(!imported & is_encoded_path(vertices))]
    config$graph <- subset_graph(config$graph, vertices)
  }
  if (!is.null(from)) {
    config$graph <- nbhd_graph(
      graph = config$graph,
      vertices = from,
      mode = match.arg(mode),
      order = order %||% igraph::gorder(config$graph)
    )
  }
  if (!is.null(subset)) {
    config$graph <- subset_graph(graph = config$graph, subset = subset)
  }
  config$import_names <- all_imports(config)
  if (targets_only) {
    config$graph <- igraph::delete_vertices(
      graph = config$graph,
      v = igraph::V(config$graph)$name[igraph::V(config$graph)$imported]
    )
  }
  config <- get_raw_node_category_data(config)
  network_data <- visNetwork::toVisNetworkData(config$graph)
  network_data$nodes
  config$nodes <- network_data$nodes[igraph::topo_sort(config$graph)$name, ]
  if (!is.null(group)) {
    config$nodes[[group]] <- get_cluster_grouping(config, group)
  }
  config <- trim_node_categories(config)
  config$nodes <- configure_nodes(config = config)
  if (show_output_files) {
    config$nodes <- append_output_file_nodes(config)
  }
  config$nodes <- coord_set(config$nodes)
  config$edges <- network_data$edges
  if (nrow(config$edges)) {
    config$edges$arrows <- "to"
  }
  if (!is.null(config$on_select_col)) {
    config$nodes$on_select_col <- get_select_col(config)
  }

  if (length(config$group)) {
    config <- cluster_nodes(config)
  }
  out <- list(
    nodes = weak_as_tibble(config$nodes),
    edges = weak_as_tibble(config$edges),
    legend_nodes = filtered_legend_nodes(
      all_nodes = config$nodes,
      full_legend = full_legend,
      font_size = font_size
    ),
    default_title = default_graph_title()
  )
  class(out) <- "drake_graph_info"
  out
}

#' @export
print.drake_graph_info <- function(x, ...) {
  cat(
    "drake graph visual info:",
    nrow(x$nodes),
    "nodes and",
    nrow(x$edges),
    "edges:\n"
  )
  min_str(x)
}

body(drake_graph_info) <- config_util_body(drake_graph_info_impl)

get_raw_node_category_data <- function(config) {
  all_labels <- V(config$graph)$name
  config$targets <- all_targets(config)
  is_dynamic <- vapply(
    config$targets,
    is_dynamic,
    FUN.VALUE = logical(1),
    config = config
  )
  config$dynamic <- config$targets[is_dynamic]
  config$outdated <- resolve_graph_outdated(config = config)
  prog <- config$cache$get_progress(config$targets)
  config$running <- config$targets[prog == "running"]
  config$cancelled <- config$targets[prog == "cancelled"]
  config$failed <- config$targets[prog == "failed"]
  config$files <- all_labels[is_encoded_path(all_labels)]
  config$functions <- parallel_filter(
    x = config$import_names,
    f = function(x) {
      is.function(get_import_from_memory(x, config = config))
    },
    jobs = config$settings$jobs_preprocess
  )
  config$missing <- parallel_filter(
    x = config$import_names,
    f = function(x) {
      missing_import(x, config = config)
    },
    jobs = config$settings$jobs_preprocess
  )
  config
}

trim_node_categories <- function(config) {
  elts <- c(
    "cancelled", "failed", "files", "functions", "running", "missing",
    "outdated", "targets"
  )
  for (elt in elts) {
    config[[elt]] <- intersect(config[[elt]], config$nodes$id)
  }
  config
}

resolve_graph_outdated <- function(config) {
  if (config$from_scratch) {
    config$outdated <- all_targets(config)
  } else {
    config$outdated <- outdated_impl(
      config = config,
      make_imports = config$make_imports
    )
  }
}

append_output_file_nodes <- function(config) {
  with(config, {
    cols <- setdiff(colnames(nodes), c("id", "label", "level", "shape", "type"))
    for (target in intersect(names(file_out), nodes$id)) {
      files <- intersect(file_out[[target]], nodes$id)
      if (length(files)) {
        for (col in cols) {
          nodes[files, col] <- nodes[target, col]
        }
      }
    }
    nodes
  })
}

resolve_build_times <- function(build_times) {
  if (is.logical(build_times)) {
    warning(
      "The build_times argument to the visualization functions ",
      "should be a character string: \"build\", \"command\", or \"none\". ",
      "The change will be forced in a later version of `drake`. ",
      "see the `build_times()` function for details."
    )
    if (build_times) {
      build_times <- "build"
    } else {
      build_times <- "none"
    }
  }
  build_times
}

get_select_col <- function(config) {
  get_cluster_grouping(config, config$on_select_col)
}

get_cluster_grouping <- function(config, group) {
  vapply(
    X = config$nodes$id,
    FUN = function(x) {
      out <- config$spec[[x]][[group]]
      if (!is.character(out)) {
        out <- safe_deparse(out, backtick = TRUE)
      }
      out %||% NA_character_
    },
    FUN.VALUE = character(1)
  )
}

#' @title Return the default title for graph visualizations
#' `r lifecycle::badge("soft-deprecated")`
#' @description For internal use only.
#' @export
#' @keywords internal
#' @return A character scalar with the default graph title.
#' @examples
#' default_graph_title()
default_graph_title <- function() {
  "Dependency graph"
}

null_graph <- function() {
  assert_pkg("visNetwork")
  nodes <- data.frame(id = 1, label = "Nothing to plot.")
  visNetwork::visNetwork(
    nodes = nodes,
    edges = data.frame(from = NA_character_, to = NA_character_),
    main = "Nothing to plot."
  )
}

filtered_legend_nodes <- function(all_nodes, full_legend, font_size) {
  legend_nodes <- legend_nodes(font_size = font_size)
  if (full_legend) {
    legend_nodes
  } else {
    filter_legend_nodes(legend_nodes = legend_nodes, all_nodes = all_nodes)
  }
}

filter_legend_nodes <- function(legend_nodes, all_nodes) {
  colors <- c(unique(all_nodes$color), node_color("object"))
  shapes <- unique(all_nodes$shape)
  ln <- legend_nodes
  ln[ln$color %in% colors & ln$shape %in% shapes, , drop = FALSE] # nolint
}

#' @title Create the nodes data frame used in the legend
#'   of the graph visualizations.
#' `r lifecycle::badge("soft-deprecated")`
#' @export
#' @description Output a `visNetwork`-friendly
#' data frame of nodes. It tells you what
#' the colors and shapes mean
#' in the graph visualizations.
#' @param font_size Font size of the node label text.
#' @return A data frame of legend nodes for the graph visualizations.
#' @examples
#' \dontrun{
#' # Show the legend nodes used in graph visualizations.
#' # For example, you may want to inspect the color palette more closely.
#' if (requireNamespace("visNetwork", quietly = TRUE)) {
#' # visNetwork::visNetwork(nodes = legend_nodes()) # nolint
#' }
#' }
legend_nodes <- function(font_size = 20) {
  out <- weak_tibble(
    label = c(
      "Up to date",
      "Outdated",
      "Running",
      "Cancelled",
      "Failed",
      "Imported",
      "Missing",
      "Object",
      "Dynamic",
      "Function",
      "File",
      "Cluster"
    ),
    color = node_color(c(
      "up_to_date",
      "outdated",
      "running",
      "cancelled",
      "failed",
      "import",
      "missing",
      rep("generic", 5)
    )),
    shape = node_shape(c(
      rep("object", 8),
      "dynamic",
      "funct",
      "file",
      "cluster"
    )),
    font.color = "black",
    font.size = font_size
  )
  out$id <- seq_len(nrow(out))
  out
}

cluster_nodes <- function(config) {
  for (cluster in config$clusters) {
    index <- config$nodes[[config$group]] == cluster
    index[is.na(index)] <- FALSE
    if (!any(index)) {
      next
    }
    new_node <- config$nodes[index, ]
    status <- cluster_status(new_node$status)
    new_node <- new_node[which.max(new_node$level), ]
    new_node$status <- status
    new_node$type <- "cluster"
    new_node <- style_nodes(
      config = list(nodes = new_node, font_size = config$font_size))
    new_node$label <- new_node$id <-
      paste0(config$group, ": ", cluster)
    if (!is.null(config$on_select_col)) {
      new_node$on_select_col <- config$nodes[index, "on_select_col"][[1]]
    }
    matching <- config$nodes$id[index]
    new_node$title <- paste(matching, collapse = ", ")
    new_node$title <- crop_text(new_node$title, width = hover_width)
    config$nodes <- rbind(config$nodes[!index, ], new_node)
    config$edges$from[config$edges$from %in% matching] <- new_node$id
    config$edges$to[config$edges$to %in% matching] <- new_node$id
  }
  config$nodes$level <- as.integer(ordered(config$nodes$level))
  config$edges <- config$edges[!duplicated(config$edges), ]
  config$edges <- config$edges[config$edges$from != config$edges$to, ]
  config
}

cluster_status <- function(statuses) {
  precedence <- c(
    "other",
    "imported",
    "up to date",
    "missing",
    "outdated",
    "running",
    "failed"
  )
  out <- "other"
  for (status in precedence) {
    if (status %in% statuses) {
      out <- status
    }
  }
  out
}

configure_nodes <- function(config) {
  rownames(config$nodes) <- config$nodes$id
  config$nodes$label <- config$cache$display_keys(config$nodes$label)
  config$nodes <- categorize_nodes(config = config)
  config$nodes <- style_nodes(config = config)
  config$nodes <- resolve_levels(config = config)
  config$nodes <- wrap_labels(config = config)
  if (config$build_times != "none") {
    config$nodes <- append_build_times(config = config)
  }
  hover_text(config = config)
}

categorize_nodes <- function(config) {
  with(config, {
    nodes$status <- "imported"
    nodes[targets, "status"] <- "up to date"
    nodes[missing, "status"] <- "missing"
    nodes[outdated, "status"] <- "outdated"
    nodes[running, "status"] <- "running"
    nodes[cancelled, "status"] <- "cancelled"
    nodes[failed, "status"] <- "failed"
    nodes$type <- "object"
    nodes[dynamic, "type"] <- "dynamic"
    nodes[is_encoded_path(nodes$id), "type"] <- "file"
    nodes[functions, "type"] <- "function"
    nodes
  })
}

resolve_levels <- function(config) {
  config$nodes$level <- level <- 0
  graph <- config$graph
  check_drake_graph(graph)
  while (length(V(graph))) {
    level <- level + 1
    leaves <- leaf_nodes(graph)
    leaves <- intersect(leaves, config$nodes$id)
    config$nodes[leaves, "level"] <- level
    graph <- igraph::delete_vertices(graph = graph, v = leaves)
  }
  config$nodes
}

wrap_labels <- function(config) {
  config$nodes$label <- hard_wrap(config$nodes$label, width = hover_width)
  config$nodes
}

style_nodes <- function(config) {
  with(config, {
    nodes$font.size <- font_size # nolint
    nodes[nodes$status == "imported", "color"] <- node_color("import")
    nodes[nodes$status == "running", "color"] <- node_color("running")
    nodes[nodes$status == "cancelled", "color"] <- node_color("cancelled")
    nodes[nodes$status == "failed", "color"] <- node_color("failed")
    nodes[nodes$status == "missing", "color"] <- node_color("missing")
    nodes[nodes$status == "outdated", "color"] <- node_color("outdated")
    nodes[nodes$status == "up to date", "color"] <- node_color("up_to_date")
    nodes[nodes$type == "object", "shape"] <- node_shape("object")
    nodes[nodes$type == "dynamic", "shape"] <- node_shape("dynamic")
    nodes[nodes$type == "file", "shape"] <- node_shape("file")
    nodes[nodes$type == "function", "shape"] <- node_shape("funct")
    nodes[nodes$type == "cluster", "shape"] <- node_shape("cluster")
    nodes
  })
}

node_color <- Vectorize(function(x) {
  color <- switch(
    x,
    default = "dodgerblue3",
    fail = "red",
    up_to_date = "forestgreen",
    outdated = "#000000",
    cancelled = "#ecb753",
    failed = "#aa0000",
    import = "dodgerblue3",
    missing = "darkorchid3",
    running = "#ff7221",
    "#888888"
  )
  col2hex(color)
},
"x", USE.NAMES = FALSE)

col2hex <- function(cname) {
  assert_pkg("grDevices")
  col_mat <- grDevices::col2rgb(cname)
  grDevices::rgb(
    red = col_mat[1, ] / 255,
    green = col_mat[2, ] / 255,
    blue = col_mat[3, ] / 255
  )
}

node_shape <- Vectorize(function(x) {
  switch(
    x,
    object = "dot",
    dynamic = "star",
    file = "square",
    funct = "triangle",
    cluster = "diamond",
    "dot"
  )
},
"x", USE.NAMES = FALSE)

append_build_times <- function(config) {
  time_data <- build_times(
    list = all_targets(config),
    digits = config$digits,
    cache = config$cache,
    type = config$build_times
  )
  nodes <- config$nodes
  timed <- intersect(time_data$target, nodes$id)
  if (!length(timed)) {
    return(nodes)
  }
  time_data <- aggregate_dynamic_times(time_data, config)
  time_labels <- as.character(time_data$elapsed)
  if (any(time_data$dynamic)) {
    time_labels[time_data$dynamic] <- paste0(
      time_data$subtargets[time_data$dynamic],
      " sub-targets\n",
      time_labels[time_data$dynamic],
      " elapsed\n(time depends on jobs)"
    )
  }
  names(time_labels) <- time_data$target
  time_labels <- time_labels[timed]
  nodes[timed, "label"] <- paste(
    nodes[timed, "label"],
    time_labels,
    sep = "\n"
  )
  nodes
}

aggregate_dynamic_times <- function(time_data, config) {
  time_data$dynamic <- vlapply(time_data$target, is_dynamic, config = config)
  time_data$subtargets <- 0L
  dynamic <- which(time_data$dynamic)
  for (i in dynamic) {
    target <- time_data$target[i]
    meta <- config$cache$get(target, namespace = "meta")
    time_data$subtargets[i] <- length(meta$subtargets)
  }
  time_data
}

hover_text <- function(config) {
  with(config, {
    if (!hover) {
      nodes$title <-
        "Call drake_graph_info(hover = TRUE) for informative text."
      return(nodes)
    }
    nodes$title <- nodes$id
    import_files <- setdiff(files, targets)
    nodes[import_files, "title"] <-
      file_hover_text(encoded_file = import_files, targets, config)
    nodes[functions, "title"] <-
      function_hover_text(function_name = functions, envir = config$envir)
    nodes[targets, "title"] <-
      target_hover_text(targets = targets, config = config)
    nodes
  })
}

file_hover_text <- Vectorize(function(encoded_file, targets, config) {
  decoded_file <- config$cache$decode_path(encoded_file)
  if (encoded_file %in% targets || !file.exists(decoded_file)) {
    return(encoded_file)
  }
  tryCatch({
    x <- readLines(decoded_file, n = 20, warn = FALSE)
    x <- crop_lines(x, n = hover_lines)
    x <- crop_text(x, width = hover_width)
    paste0(x, collapse = "<br>")
  },
  error = function(e) encoded_file,
  warning = function(w) encoded_file
  )
},
"encoded_file")

function_hover_text <- Vectorize(function(function_name, envir) {
  x <- tryCatch(
    get(x = function_name, envir = envir),
    error = function(e) function_name
  )
  x <- unwrap_function(x)
  style_hover_text(x)
},
"function_name")

target_hover_text <- function(targets, config) {
  commands <- vapply(
    X = targets,
    FUN = function(target) {
      config$spec[[target]]$command_standardized
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
  vapply(
    X = commands,
    FUN = style_hover_text,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}

# I would use styler for indentation here,
# but it adds a lot of processing time
# for large functions.
style_hover_text <- function(x) {
  if (!is.character(x)) {
    x <- safe_deparse(x, backtick = TRUE)
  }
  x <- crop_lines(x, n = hover_lines)
  x <- crop_text(x, width = hover_width)
  x <- gsub(pattern = " ", replacement = "&nbsp;", x = x)
  x <- gsub(pattern = "\n", replacement = "<br>", x = x)
  paste0(x, collapse = "<br>")
}

crop_text <- Vectorize(function(x, width = getOption("width")) {
  if (nchar(x) > width) {
    x <- paste0(substr(x, 1, width - 3), "...")
  }
  x
},
"x", USE.NAMES = FALSE)

hover_lines <- 10
hover_width <- 49

crop_lines <- function(x, n = 10) {
  if (length(x) > n) {
    x <- x[1:(n - 1)]
    x[n] <- "..."
  }
  x
}

coord_set <- function(nodes) {
  nodes <- coord_x(nodes)
  nodes <- coord_y(nodes)
  nodes
}

coord_x <- function(nodes, min = -1, max = 1) {
  nodes$x <- coord_rescale(nodes$level, min = min, max = max)
  nodes
}

coord_y <- function(nodes, min = -1, max = 1) {
  splits <- base::split(nodes, nodes$x)
  out <- lapply(splits, coord_y_stage, min = min, max = max)
  out <- do.call(rbind, out)
  out$y <- coord_rescale(out$y, min = min, max = max)
  out
}

coord_y_stage <- function(nodes, min, max) {
  y <- seq(from = min, to = max, length.out = nrow(nodes) + 2)
  nodes$y <- y[c(-1, -length(y))]
  nodes
}

coord_rescale <- function(x, min, max) {
  x <- x - min(x)
  x <- x / max(x)
  x <- x * (max - min)
  x <- x + min
  x [!is.finite(x)] <- (min + max) / 2
  x
}
