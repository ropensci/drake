# For hover text
hover_text_length <- 250

append_build_times <- function(config) {
  with(config, {
    time_data <- build_times(digits = digits, cache = config$cache)
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

arrange_nodes <- function(config){
  if (config$parallelism %in% parallelism_choices(distributed_only = TRUE))
    resolve_levels_distributed(config = config)
  else
    resolve_levels(config = config)
}

shrink_levels <- function(nodes){
  nodes <- nodes[order(nodes$level), ]
  nodes$level <- c(0, cumsum(diff(nodes$level) > 0))
  nodes
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
    nodes[is_file(nodes$id), "type"] <- "file"
    nodes[functions, "type"] <- "function"
    nodes
  })
}

configure_nodes <- function(config){
  elts <- c(
    "failed", "functions", "in_progress", "missing",
    "outdated", "targets"
  )
  for (elt in elts){
    config[[elt]] <- intersect(config[[elt]], config$nodes$id)
  }
  config$nodes <- categorize_nodes(config = config)
  config$nodes <- arrange_nodes(config = config)
  config$nodes <- style_nodes(config = config)
  if (config$build_times){
    config$nodes <- append_build_times(config = config)
  }
  hover_text(config = config)
}

#' @title Function default_graph_title
#' @description Default title of the graph for
#' \code{\link{plot_graph}()}.
#' @export
#' @seealso \code{\link{dataframes_graph}}, \code{\link{plot_graph}}
#' @param parallelism Mode of parallelism intended for the workplan.
#' See \code{\link{parallelism_choices}()}.
#' @param split_columns logical, whether the columns were split
#' in \code{\link{dataframes_graph}()} or \code{\link{plot_graph}()}
#' with the \code{split_columns} argument.
#' @examples
#' default_graph_title()
default_graph_title <- function(
  parallelism = drake::parallelism_choices(distributed_only = FALSE),
  split_columns = FALSE
){
  parallelism <- match.arg(parallelism)
  out <- paste("Workflow graph:", parallelism, "parallelism")
  if (split_columns){
    out <- paste(out, "with split columns")
  }
  out
}

file_hover_text <- Vectorize(function(quoted_file, targets){
  unquoted_file <- unquote(quoted_file)
  if (quoted_file %in% targets | !file.exists(unquoted_file))
    return(quoted_file)
  tryCatch({
    readLines(unquoted_file, n = 10, warn = FALSE) %>%
      paste(collapse = "\n") %>%
      crop_text(length = hover_text_length)
  },
  error = function(e) quoted_file,
  warning = function(w) quoted_file
  )
},
"quoted_file")

function_hover_text <- Vectorize(function(function_name, envir){
  tryCatch(
    eval(parse(text = function_name), envir = envir),
    error = function(e) function_name) %>%
    unwrap_function %>%
    deparse %>%
    paste(collapse = "\n") %>%
    crop_text(length = hover_text_length)
},
"function_name")

target_hover_text <- function(targets, plan) {
  plan[plan$target %in% targets, "command"] %>%
    wrap_text %>% crop_text(length = hover_text_length)
}

hover_text <- function(config) {
  with(config, {
    nodes$hover_label <- nodes$id
    import_files <- setdiff(files, targets)
    nodes[import_files, "hover_label"] <-
      file_hover_text(quoted_file = import_files, targets = targets)
    nodes[functions, "hover_label"] <-
      function_hover_text(function_name = functions, envir = config$envir)
    nodes[targets, "hover_label"] <-
      target_hover_text(targets = targets, plan = config$plan)
    nodes
  })
}

null_graph <- function() {
  nodes <- data.frame(id = 1, label = "Nothing to plot.")
  visNetwork::visNetwork(
    nodes = nodes, edges = data.frame(from = NA, to = NA))
}

missing_import <- function(x, envir) {
  missing_object <- !is_file(x) & is.null(envir[[x]]) & tryCatch({
    flexible_get(x)
    FALSE
  },
  error = function(e) TRUE)
  missing_file <- is_file(x) & !file.exists(unquote(x))
  missing_object | missing_file
}

resolve_levels <- function(config) {
  with(config, {
    stopifnot(is_dag(graph))
    level <- 1
    nodes$level <- NA
    keep_these <- setdiff(V(graph)$name, rownames(nodes))
    graph_remaining_targets <- delete_vertices(graph, v = keep_these)
    while (length(V(graph_remaining_targets))) {
      candidates <- next_targets(graph_remaining_targets, jobs = jobs)
      if (length(candidates)){
        nodes[candidates, "level"] <- level
        level <- level + 1
      }
      graph_remaining_targets <-
        delete_vertices(graph_remaining_targets, v = candidates)
    }
    nodes
  })
}

resolve_levels_distributed <- function(config) { # nolint
  with(config, {
    targets <- intersect(plan$target, nodes$id)
    imports <- setdiff(nodes$id, targets)
    if (!length(targets) | !length(imports)){
      return(resolve_levels(config))
    }
    graph_imports <- delete_vertices(graph, v = targets)
    graph_targets <- delete_vertices(graph, v = imports)
    nodes_imports <- nodes[nodes$id %in% imports, ]
    nodes_targets <- nodes[nodes$id %in% targets, ]
    nodes_imports <- resolve_levels(
      config = list(
        nodes = nodes_imports,
        graph = graph_imports,
        jobs = config$jobs
      )
    )
    nodes_targets <- resolve_levels(
      config = list(
        nodes = nodes_targets,
        graph = graph_targets,
        jobs = config$jobs
      )
    )
    nodes_imports$level <- nodes_imports$level - max(nodes_imports$level)
    rbind(nodes_imports, nodes_targets)
  })
}

# https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
split_levels <- function(old_levels, max_reps){
  n_nodes <- length(old_levels)
  n_levels <- floor(n_nodes / max_reps) + (n_nodes %% max_reps > 0)
  index <- seq_along(old_levels)
  new_levels <- split(index, sort(index %% n_levels)) %>%
    lapply(FUN = function(substage){
      rep(max(substage), length(substage))
    }) %>%
    unlist %>%
    unname
  new_levels <- new_levels - min(new_levels)
  new_levels / (max(new_levels) + 1) + min(old_levels)
}

split_node_columns <- function(nodes){
  max_reps <- nrow(nodes) %>%
    sqrt %>%
    ceiling
  out <- ddply(nodes, "level", function(stage){
    stage$level <- split_levels(
      old_levels = stage$level, max_reps = max_reps)
    stage
  })
  rownames(out) <- out$id
  out$level <- as.integer(as.factor(out$level))
  out
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
    nodes
  })
}

subset_nodes_edges <- function(config, keep, choices = V(config$graph)$name){
  keep <- sanitize_nodes(nodes = keep, choices = choices)
  config$nodes <- config$nodes[keep, ]
  config$edges <-
    config$edges[
      config$edges$from %in% keep &
      config$edges$to %in% keep, ]
  config
}

#' @title Function legend_nodes
#' @export
#' @seealso \code{\link{drake_palette}()},
#' \code{\link{plot_graph}()},
#' \code{\link{dataframes_graph}()}
#' @description Output a \code{visNetwork}-friendly
#' data frame of nodes. It tells you what
#' the colors and shapes mean
#' in \code{\link{plot_graph}()}.
#' @param font_size font size of the node label text
#' @examples
#' \dontrun{
#' # Show the legend nodes used in plot_graph().
#' # For example, you may want to inspect the color palette more closely.
#' visNetwork::visNetwork(nodes = legend_nodes())
#' }
legend_nodes <- function(font_size = 20) {
  out <- data.frame(
    label = c(
      "Up to date",
      "Outdated",
      "In progress",
      "Failed",
      "Imported",
      "Missing",
      "Object",
      "Function",
      "File"
    ),
    color = color_of(c(
      "up_to_date",
      "outdated",
      "in_progress",
      "failed",
      "import_node",
      "missing_node",
      rep("generic", 3)
    )),
    shape = shape_of(c(
      rep("object", 7),
      "funct",
      "file"
    )),
    font.color = "black",
    font.size = font_size
  )
  out$id <- seq_len(nrow(out))
  out
}
