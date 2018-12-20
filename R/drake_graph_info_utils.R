hover_lines <- 10
hover_width <- 49

append_build_times <- function(config) {
  with(config, {
    time_data <- build_times(
      digits = digits, cache = cache, type = build_times)
    timed <- intersect(time_data$item, nodes$id)
    if (!length(timed))
      return(nodes)
    time_labels <- as.character(time_data$elapsed)
    names(time_labels) <- time_data$item
    time_labels <- time_labels[timed]
    nodes[timed, "label"] <-
      paste(nodes[timed, "label"], time_labels, sep = "\n")
    nodes
  })
}

append_output_file_nodes <- function(config) {
  with(config, {
    cols <- setdiff(colnames(nodes), c("id", "label", "level", "shape"))
    for (target in intersect(names(file_out), nodes$id)) {
      files <- intersect(file_out[[target]], nodes$id)
      if (length(files)){
        for (col in cols) {
          nodes[files, col] <- nodes[target, col]
        }
      }
    }
    nodes
  })
}

can_get_function <- function(x, envir) {
  tryCatch({
    is.function(eval(parse(text = x), envir = envir))
  },
  error = function(e) FALSE)
}

categorize_nodes <- function(config) {
  with(config, {
    nodes$status <- "imported"
    nodes[targets, "status"] <- "up to date"
    nodes[missing, "status"] <- "missing"
    nodes[outdated, "status"] <- "outdated"
    nodes[in_progress, "status"] <- "in progress"
    nodes[failed, "status"] <- "failed"
    nodes$type <- "object"
    nodes[is_encoded_path(nodes$id), "type"] <- "file"
    nodes[functions, "type"] <- "function"
    nodes
  })
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
    "in progress",
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
  rownames(config$nodes) <- config$nodes$label
  config$nodes <- categorize_nodes(config = config)
  config$nodes <- style_nodes(config = config)
  config$nodes <- resolve_levels(config = config)
  if (config$build_times != "none") {
    config$nodes <- append_build_times(config = config)
  }
  hover_text(config = config)
}

#' @title Return the default title for graph visualizations
#' @description For internal use only.
#' @export
#' @keywords internal
#' @return A character scalar with the default graph title.
#' @param split_columns deprecated
#' @examples
#' default_graph_title()
default_graph_title <- function(split_columns = FALSE) {
  "Dependency graph"
}

file_hover_text <- Vectorize(function(encoded_file, targets) {
  decoded_file <- decoded_path(encoded_file)
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

filter_legend_nodes <- function(legend_nodes, all_nodes) {
  colors <- c(unique(all_nodes$color), color_of("object"))
  shapes <- unique(all_nodes$shape)
  ln <- legend_nodes
  ln[ln$color %in% colors & ln$shape %in% shapes, , drop = FALSE] # nolint
}

filtered_legend_nodes <- function(all_nodes, full_legend, font_size) {
  legend_nodes <- legend_nodes(font_size = font_size)
  if (full_legend) {
    legend_nodes
  } else {
    filter_legend_nodes(legend_nodes = legend_nodes, all_nodes = all_nodes)
  }
}

function_hover_text <- Vectorize(function(function_name, envir) {
  x <- tryCatch(
    get(x = function_name, envir = envir),
    error = function(e) function_name
  )
  x <- unwrap_function(x)
  x <- deparse(x)
  style_hover_text(x)
},
"function_name")

get_raw_node_category_data <- function(config) {
  all_labels <- V(config$graph)$name
  config$outdated <- resolve_graph_outdated(config = config)
  config$in_progress <- in_progress(cache = config$cache)
  config$failed <- failed(cache = config$cache)
  config$files <- parallel_filter(
    x = all_labels, f = is_encoded_path, jobs = config$jobs)
  config$functions <- parallel_filter(
    x = config$imports,
    f = function(x) can_get_function(x, envir = config$envir),
    jobs = config$jobs
  )
  config$missing <- parallel_filter(
    x = config$imports,
    f = function(x) missing_import(x, envir = config$envir),
    jobs = config$jobs
  )
  config
}

hover_text <- function(config) {
  with(config, {
    nodes$title <- nodes$id
    import_files <- setdiff(files, targets)
    nodes[import_files, "title"] <-
      file_hover_text(encoded_file = import_files, targets = targets)
    nodes[functions, "title"] <-
      function_hover_text(function_name = functions, envir = config$envir)
    nodes[targets, "title"] <-
      target_hover_text(targets = targets, plan = config$plan)
    nodes
  })
}

#' @title Create the nodes data frame used in the legend
#'   of the graph visualizations.
#' @export
#' @description Output a `visNetwork`-friendly
#' data frame of nodes. It tells you what
#' the colors and shapes mean
#' in the graph visualizations.
#' @param font_size font size of the node label text
#' @return A data frame of legend nodes for the graph visualizations.
#' @examples
#' \dontrun{
#' # Show the legend nodes used in graph visualizations.
#' # For example, you may want to inspect the color palette more closely.
#' visNetwork::visNetwork(nodes = legend_nodes()) # nolint
#' }
legend_nodes <- function(font_size = 20) {
  out <- weak_tibble(
    label = c(
      "Up to date",
      "Outdated",
      "In progress",
      "Failed",
      "Imported",
      "Missing",
      "Object",
      "Function",
      "File",
      "Cluster"
    ),
    color = color_of(c(
      "up_to_date",
      "outdated",
      "in_progress",
      "failed",
      "import_node",
      "missing_node",
      rep("generic", 4)
    )),
    shape = shape_of(c(
      rep("object", 7),
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

missing_import <- function(x, envir) {
  missing_object <- !is_encoded_path(x) & is.null(envir[[x]]) & tryCatch({
    flexible_get(x, envir = envir)
    FALSE
  },
  error = function(e) TRUE)
  missing_file <- is_encoded_path(x) & !file.exists(decoded_path(x))
  missing_object | missing_file
}

null_graph <- function() {
  assert_pkg("visNetwork")
  nodes <- data.frame(id = 1, label = "Nothing to plot.")
  visNetwork::visNetwork(
    nodes = nodes,
    edges = data.frame(from = NA, to = NA),
    main = "Nothing to plot."
  )
}

# I would use styler for indentation here,
# but it adds a lot of processing time
# for large functions.
style_hover_text <- function(lines) {
  x <- crop_lines(lines, n = hover_lines)
  x <- crop_text(x, width = hover_width)
  x <- gsub(pattern = " ", replacement = "&nbsp;", x = x)
  x <- gsub(pattern = "\n", replacement = "<br>", x = x)
  paste0(x, collapse = "<br>")
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

resolve_graph_outdated <- function(config) {
  if (config$from_scratch) {
    config$outdated <- config$plan$target
  } else {
    config$outdated <- outdated(
      config = config,
      make_imports = config$make_imports
    )
  }
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

style_nodes <- function(config) {
  with(config, {
    nodes$font.size <- font_size # nolint
    nodes[nodes$status == "imported", "color"] <- color_of("import_node")
    nodes[nodes$status == "in progress", "color"] <- color_of("in_progress")
    nodes[nodes$status == "failed", "color"] <- color_of("failed")
    nodes[nodes$status == "missing", "color"] <- color_of("missing_node")
    nodes[nodes$status == "outdated", "color"] <- color_of("outdated")
    nodes[nodes$status == "up to date", "color"] <- color_of("up_to_date")
    nodes[nodes$type == "object", "shape"] <- shape_of("object")
    nodes[nodes$type == "file", "shape"] <- shape_of("file")
    nodes[nodes$type == "function", "shape"] <- shape_of("funct")
    nodes[nodes$type == "cluster", "shape"] <- shape_of("cluster")
    nodes
  })
}

target_hover_text <- function(targets, plan) {
  vapply(
    X = plan$command[plan$target %in% targets],
    FUN = style_hover_text,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}

trim_node_categories <- function(config) {
  elts <- c(
    "failed", "files", "functions", "in_progress", "missing",
    "outdated", "targets"
  )
  for (elt in elts) {
    config[[elt]] <- intersect(config[[elt]], config$nodes$id)
  }
  config
}
