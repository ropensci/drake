sanitize_plan <- function(plan){
  for (field in c("code", "command", "output", "target")){
    if (!is.null(plan[[field]])){
      plan[[field]] <- str_trim(plan[[field]], side = "both")
    }
  }
  as.data.frame(plan, stringsAsFactors = FALSE) %>%
    fix_deprecated_plan_names()
}

sanitize_targets <- function(plan, targets){
  plan <- sanitize_plan(plan)
  targets <- str_trim(targets, side = "both")
  sanitize_nodes(nodes = targets, choices = plan$target)
}

sanitize_from <- function(config){
  choices <- V(config$graph)$name
  if (length(config$from)){
    config$from <-
      sanitize_nodes(nodes = config$from, choices = choices)
  }
  config
}

sanitize_nodes <- function(nodes, choices){
  if (!any(nodes %in% choices)){
    stop(
      "All import/target names are invalid ",
      "in argument 'targets', 'to', or 'from' ",
      "for make(), plot_graph(), or similar.",
      call. = FALSE
    )
  }
  diffs <- setdiff(nodes, choices)
  if (length(diffs)){
    warning(
      "Ignoring imports/targets that were requeted but not found:\n",
      multiline_message(diffs),
      call. = FALSE
    )
  }
  intersect(nodes, choices)
}
