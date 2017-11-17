sanitize_plan <- function(plan){
  for (field in workplan_columns()){
    if (!is.null(plan[[field]])){
      plan[[field]] <- str_trim(plan[[field]], side = "both")
    }
  }
  plan <- as.data.frame(plan, stringsAsFactors = FALSE)
  if (!is.null(plan$trigger)){
    assert_legal_triggers(plan$trigger)
  }
  plan[nchar(plan$target) > 0, ]
}

workplan_columns <- function(){
  c(
    "cpu",
    "command",
    "elapsed",
    "retries",
    "target",
    "timeout",
    "trigger"
  )
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
      "in argument 'targets', 'from', or 'subset' ",
      "for make(), vis_drake_graph(), or similar.",
      call. = FALSE
    )
  }
  diffs <- setdiff(nodes, choices)
  if (length(diffs)){
    warning(
      "Ignoring imports/targets that were requested but not found:\n",
      multiline_message(diffs),
      call. = FALSE
    )
  }
  intersect(nodes, choices) %>%
    unique
}
