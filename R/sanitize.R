sanitize_plan <- function(plan){
  for (field in drake_plan_character_columns()){
    if (!is.null(plan[[field]])){
      plan[[field]] <- str_trim(plan[[field]], side = "both")
    }
  }
  if (!is.null(plan[["trigger"]])){
    assert_legal_triggers(plan[["trigger"]])
  }
  plan[nchar(plan$target) > 0, ]
}

drake_plan_character_columns <- function(){
  c(
    "command",
    "target",
    "trigger"
  )
}

drake_plan_columns <- function(){
  c(
    drake_plan_character_columns(),
    "cpu",
    "elapsed",
    "evaluator",
    "retries",
    "timeout"
  )
}

sanitize_targets <- function(plan, targets){
  plan <- sanitize_plan(plan)
  targets <- str_trim(targets, side = "both")
  sanitize_nodes(nodes = targets, choices = plan$target)
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
