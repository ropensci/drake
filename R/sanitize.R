sanitize_plan <- function(plan){
  for (field in drake_plan_character_columns()){
    if (!is.null(plan[[field]])){
      plan[[field]] <- str_trim(plan[[field]], side = "both")
    }
  }
  if (!is.null(plan[["trigger"]])){
    assert_legal_triggers(plan[["trigger"]])
  }
  plan$target <- repair_target_names(plan$target)
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
  targets <- repair_target_names(targets)
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

repair_target_names <- function(x){
  illegals <- c(
    ":", "\\+", "\\-", "\\*", "\\^",
    "\\(", "\\)", "\\[", "\\]", "^_"
  ) %>%
    paste(collapse = "|")
  non_files <- x[is_not_file(x)]
  if (any(grepl(illegals, non_files))){
    warning("replacing illegal symbols in target names with '_'.")
  } else {
    return(x)
  }
  x <- str_trim(x, side = "both")
  x[is_not_file(x)] <- gsub(illegals, "_", x[is_not_file(x)])
  x <- gsub("^_", "", x)
  x[!nchar(x)] <- "X"
  make.unique(x, sep = "_")
}
