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
  rownames(config$nodes) <- config$nodes$label
  config$nodes <- categorize_nodes(config = config)
  config$nodes <- resolve_levels(config = config)
  config$nodes <- style_nodes(config = config)
  if (config$build_times != "none"){
    config$nodes <- append_build_times(config = config)
  }
  if (config$split_columns){
    config$nodes <- split_node_columns(nodes = config$nodes)
  }
  config$nodes <- shrink_levels(config$nodes)
  hover_text(config = config)
}

#' @title Return the default title of the graph for
#'   [vis_drake_graph()].
#' @description For internal use only.
#' @export
#' @keywords internal
#' @seealso [dataframes_graph()], [vis_drake_graph()]
#' @return a character scalar with the default graph title for
#'   [vis_drake_graph()].
#' @param split_columns logical, whether the columns were split
#'   in [dataframes_graph()] or [vis_drake_graph()]
#'   with the `split_columns` argument.
#' @examples
#' default_graph_title()
default_graph_title <- function(split_columns = FALSE){
  out <- "Workflow graph"
  if (split_columns){
    out <- paste(out, "with split columns")
  }
  out
}

file_hover_text <- Vectorize(function(quoted_file, targets){
  unquoted_file <- drake_unquote(quoted_file)
  if (quoted_file %in% targets | !file.exists(unquoted_file))
    return(quoted_file)
  tryCatch({
    readLines(unquoted_file, n = 10, warn = FALSE) %>%
      paste(collapse = "\n") %>%
      crop_text(width = hover_text_width)
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
    crop_text(width = hover_text_width)
},
"function_name")

get_raw_node_category_data <- function(config){
  all_labels <- V(config$graph)$name
  config$outdated <- resolve_graph_outdated(config = config)
  config$imports <- setdiff(all_labels, config$plan$target)
  config$in_progress <- in_progress(cache = config$cache)
  config$failed <- failed(cache = config$cache)
  config$files <- parallel_filter(
    x = all_labels, f = is_file, jobs = config$jobs)
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

graphing_parallel_stages <- function(config){
  stages <- parallel_stages(config, from_scratch = TRUE)
  rownames(stages) <- stages$item
  stages
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

hover_text_width <- 250

#' @title Create the nodes data frame used in the legend
#'   of [vis_drake_graph()].
#' @export
#' @seealso [drake_palette()],
#'   [vis_drake_graph()],
#'   [dataframes_graph()]
#' @description Output a `visNetwork`-friendly
#' data frame of nodes. It tells you what
#' the colors and shapes mean
#' in [vis_drake_graph()].
#' @param font_size font size of the node label text
#' @return A data frame of legend nodes for [vis_drake_graph()].
#' @examples
#' \dontrun{
#' # Show the legend nodes used in vis_drake_graph().
#' # For example, you may want to inspect the color palette more closely.
#' visNetwork::visNetwork(nodes = legend_nodes())
#' }
legend_nodes <- function(font_size = 20) {
  out <- tibble(
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

missing_import <- function(x, envir) {
  missing_object <- !is_file(x) & is.null(envir[[x]]) & tryCatch({
    flexible_get(x)
    FALSE
  },
  error = function(e) TRUE)
  missing_file <- is_file(x) & !file.exists(drake_unquote(x))
  missing_object | missing_file
}

null_graph <- function() {
  nodes <- data.frame(id = 1, label = "Nothing to plot.")
  visNetwork::visNetwork(
    nodes = nodes,
    edges = data.frame(from = NA, to = NA),
    main = "Nothing to plot."
  )
}

resolve_graph_outdated <- function(config){
  if (config$from_scratch){
    config$outdated <- config$plan$target
  } else {
    config$outdated <- outdated(
      config = config,
      make_imports = config$make_imports
    )
  }
}

resolve_build_times <- function(build_times){
  if (is.logical(build_times)){
    warning(
      "The build_times argument to the visualization functions ",
      "should be a character string: \"build\", \"command\", or \"none\". ",
      "The change will be forced in a later version of `drake`. ",
      "see the `build_times()` function for details."
    )
    if (build_times){
      build_times <- "build"
    } else {
      build_times <- "none"
    }
  }
  build_times
}

resolve_levels <- function(config) { # nolint
  with(config, {
    stages <- stages[nodes$id, ]
    nodes$level <- stages[rownames(nodes), "stage"]
    nodes
  })
}

shrink_levels <- function(nodes){
  nodes <- nodes[order(nodes$level), ]
  nodes$level <- c(0, cumsum(diff(nodes$level) > 0))
  nodes
}

# https://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
split_levels <- function(old_levels, max_reps){
  n_nodes <- length(old_levels)
  n_levels <- floor(n_nodes / max_reps) + (n_nodes %% max_reps > 0)
  index <- seq_along(along.with = old_levels)
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

target_hover_text <- function(targets, plan) {
  plan[plan$target %in% targets, "command"] %>%
    wrap_text %>% crop_text(width = hover_text_width)
}

trim_node_categories <- function(config){
  elts <- c(
    "failed", "files", "functions", "in_progress", "missing",
    "outdated", "targets"
  )
  for (elt in elts){
    config[[elt]] <- intersect(config[[elt]], config$nodes$id)
  }
  config
}
