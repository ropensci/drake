# For hover text
hover_text_length <- 250

# colors in graphs
generic_color <- "gray"
import_color <- "#1874cd"
in_progress_color <- "#ff7221"
missing_color <- "#9a32cd"
outdated_color <- "#aa0000"
up_to_date_color <- "#228b22"

# shapes in graph
file_shape <- "square"
function_shape <- "triangle"
generic_shape <- "dot"

append_build_times <- function(nodes, cache) {
  time_data <- build_times(cache = cache)
  timed <- intersect(time_data$target, nodes$id)
  if (!length(timed))
    return(nodes)
  time_labels <- as.character(time_data$elapsed)
  names(time_labels) <- time_data$target
  time_labels <- time_labels[timed]
  nodes[timed, "label"] <-
    paste(nodes[timed, "label"], time_labels, sep = "\n")
  nodes
}

arrange_nodes <- function(nodes, parallelism, graph, targets, imports){
  if (parallelism == "Makefile")
    resolve_levels_Makefile(nodes = nodes, graph = graph, imports = imports,
      targets = targets) else resolve_levels(nodes = nodes, graph = graph)
}

can_get_function <- function(x, envir) {
  tryCatch({
    is.function(eval(parse(text = x), envir = envir))
  },
  error = function(e) FALSE)
}

categorize_nodes <- function(nodes, functions, imports,
  in_progress, missing, outdated, targets) {
  nodes$status <- "imported"
  nodes[targets, "status"] <- "up to date"
  nodes[missing, "status"] <- "missing"
  nodes[outdated, "status"] <- "outdated"
  nodes[in_progress, "status"] <- "in progress"

  nodes$type <- "object"
  nodes[is_file(nodes$id), "type"] <- "file"
  nodes[functions, "type"] <- "function"
  nodes
}

configure_nodes <- function(nodes, plan, envir, parallelism, graph, cache,
  files, functions, imports, in_progress, missing, outdated, targets,
  font_size, build_times) {
  force(envir)
  functions <- intersect(nodes$id, functions)
  in_progress <- intersect(in_progress, nodes$id)
  missing <- intersect(nodes$id, missing)
  outdated <- intersect(outdated, nodes$id)
  targets <- intersect(nodes$id, plan$target)

  nodes <- categorize_nodes(nodes = nodes, functions = functions,
    imports = imports, in_progress = in_progress, missing = missing,
    outdated = outdated, targets = targets)
  nodes <- arrange_nodes(nodes = nodes, parallelism = parallelism,
    graph = graph, imports = imports, targets = targets)
  nodes <- style_nodes(nodes, font_size = font_size)
  if (build_times)
    nodes <- append_build_times(nodes = nodes, cache = cache)
  hover_text(nodes = nodes, plan = plan, envir = envir,
    files = files, functions = functions, targets = targets)
}

#' @title Function default_graph_title
#' @description Default title of the graph from
#' \code{\link{plot_graph}()}.
#' @export
#' @seealso \code{\link{dataframes_graph}}, \code{\link{plot_graph}}
#' @param parallelism Mode of parallelism intended for the workflow.
#' See \code{\link{parallelism_choices}()}.
#' @param split_columns logical, whether the columns were split
#' in \code{\link{dataframes_graph}()} or \code{\link{plot_graph}()}
#' with the \code{split_columns} argument.
default_graph_title <- function(
  parallelism = drake::parallelism_choices(),
  split_columns){
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
        deparse %>%
        paste(collapse = "\n") %>%
        crop_text(length = hover_text_length)
},
"function_name")

hover_text <- function(nodes, plan, targets, files, functions, envir) {
  nodes$hover_label <- nodes$id
  import_files <- setdiff(files, targets)
  nodes[import_files, "hover_label"] <-
    file_hover_text(quoted_file = import_files, targets = targets)
  nodes[functions, "hover_label"] <-
    function_hover_text(function_name = functions, envir = envir)
  nodes[targets, "hover_label"] <-
    plan[plan$target %in% targets, "command"] %>%
    wrap_text %>% crop_text(length = hover_text_length)
  nodes
}

legend_nodes <- function(font_size = 20) {
  out <- data.frame(
    label = c(
      "Up to date", "In progress", "Outdated", "Imported",
      "Missing", "Object", "Function", "File"),
    color = c(
      up_to_date_color, in_progress_color,
      outdated_color, import_color, missing_color,
      rep(generic_color, 3)),
    shape = c(rep(generic_shape, 6),
      function_shape, file_shape),
      font.color = "black",
      font.size = font_size)
  out$id <- seq_len(nrow(out))
  out
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

resolve_levels <- function(nodes, graph) {
  stopifnot(is_dag(graph))
  level <- 1
  nodes$level <- NA
  graph_remaining_targets <- graph
  while (length(V(graph_remaining_targets))) {
    candidates <- next_targets(graph_remaining_targets)
    nodes[candidates, "level"] <- level
    level <- level + 1
    graph_remaining_targets <-
      delete_vertices(graph_remaining_targets, v = candidates)
  }
  stopifnot(all(!is.na(nodes$level)))
  nodes
}

resolve_levels_Makefile <- function(nodes, graph, imports, targets) { # nolint
  graph_imports <- delete_vertices(graph, v = targets)
  graph_targets <- delete_vertices(graph, v = imports)
  nodes_imports <- nodes[nodes$id %in% imports, ]
  nodes_targets <- nodes[nodes$id %in% targets, ]
  nodes_imports <-
    resolve_levels(nodes = nodes_imports, graph = graph_imports)
  nodes_targets <-
    resolve_levels(nodes = nodes_targets, graph = graph_targets)
  nodes_imports$level <- nodes_imports$level - max(nodes_imports$level)
  rbind(nodes_imports, nodes_targets)
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
  out$level <- as.integer(as.factor(out$level))
  out
}

style_nodes <- function(nodes, font_size) {
  nodes$font.size <- font_size # nolint
  nodes[nodes$status == "imported", "color"] <- import_color
  nodes[nodes$status == "in progress", "color"] <- in_progress_color
  nodes[nodes$status == "missing", "color"] <- missing_color
  nodes[nodes$status == "outdated", "color"] <- outdated_color
  nodes[nodes$status == "up to date", "color"] <- up_to_date_color
  nodes[nodes$type == "object", "shape"] <- generic_shape
  nodes[nodes$type == "file", "shape"] <- file_shape
  nodes[nodes$type == "function", "shape"] <- function_shape
  nodes
}
