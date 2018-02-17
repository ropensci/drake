sanitize_plan <- function(plan){
  plan <- as_tibble(plan)
  for (field in drake_plan_non_factors()){
    if (!is.null(plan[[field]])){
      if (is.factor(plan[[field]])){
         plan[[field]] <- as.character(plan[[field]])
      }
      if (is.character(plan[[field]])){
        plan[[field]] <- str_trim(plan[[field]], side = "both")
      }
    }
  }
  if (!is.null(plan[["trigger"]])){
    assert_legal_triggers(plan[["trigger"]])
  }
  plan <- file_outputs_to_targets(plan)
  plan$target <- repair_target_names(plan$target)
  plan[nchar(plan$target) > 0, ]
}

drake_plan_non_factors <- function(){
  c(
    "command",
    "target",
    "trigger"
  )
}

drake_plan_columns <- function(){
  c(
    drake_plan_non_factors(),
    "cpu",
    "elapsed",
    "evaluator",
    "retries",
    "timeout",
    "evaluator"
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
    "\\(", "\\)", "\\[", "\\]", "^_",
    "\\\""
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

file_outputs_to_targets <- function(plan){
  index <- grepl("file_output", plan$command)
  plan$target[index] <- vapply(
    plan$command[index],
    single_file_output,
    character(1)
  )
  plan$target[is_file(plan$target)] <-
    plan$target[is_file(plan$target)] %>%
    gsub(pattern = "^'|'$", replacement = "\"")
  plan
}

single_file_output <- function(command){
  file_output <- command_dependencies(command)$file_output
  stopifnot(length(file_output) > 0)
  if (length(file_output) > 1){
    warning(
      "Multiple file outputs found for command `", command, "`. ",
      "Choosing ", file_output[1], " as the target name.",
      call. = FALSE
    )
    file_output[1]
  } else {
    file_output
  }
}
