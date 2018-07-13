sanitize_plan <- function(plan, allow_duplicated_targets = FALSE){
  wildcards <- attr(plan, "wildcards")
  plan <- as_tibble(plan)
  for (field in drake_plan_non_factors()){
    if (!is.null(plan[[field]])){
      if (is.factor(plan[[field]])){
         plan[[field]] <- as.character(plan[[field]])
      }
      if (is.character(plan[[field]])){
        plan[[field]] <- stringi::stri_trim_both(plan[[field]])
      }
    }
  }
  if ("trigger" %in% colnames(plan)){
    plan$trigger <- parse_triggers(plan$trigger)
  }
  plan$target <- repair_target_names(plan$target)
  plan <- plan[nzchar(plan$target), ]
  plan$command[is.na(plan$command)] <- ""
  first <- c("target", "command")
  cols <- c(first, setdiff(colnames(plan), first))
  if (!allow_duplicated_targets) {
    plan <- handle_duplicated_targets(plan[, cols])
  }
  structure(plan, wildcards = wildcards)
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
    "priority",
    "retries",
    "timeout",
    "worker"
  )
}

sanitize_targets <- function(plan, targets){
  targets <- repair_target_names(targets)
  sanitize_nodes(nodes = targets, choices = plan$target)
}

sanitize_nodes <- function(nodes, choices){
  if (!any(nodes %in% choices)){
    stop(
      "All import/target names are invalid ",
      "in argument 'targets', 'from', or 'subset' ",
      "for make() or similar function.",
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
  x <- stringi::stri_trim_both(x)
  illegals <- c(
    ":", "\\+", "\\-", "\\*", "\\^",
    "\\(", "\\)", "\\[", "\\]", "^_",
    "\\\"", "\\s+"
  ) %>%
    paste(collapse = "|")
  non_files <- x[is_not_file(x)]
  if (any(grepl(illegals, non_files))){
    warning("replacing illegal symbols in target names with '_'.")
  } else {
    return(x)
  }
  x[is_not_file(x)] <- gsub(illegals, "_", x[is_not_file(x)])
  x <- gsub("^_", "", x)
  x[!nzchar(x)] <- "X"
  make.unique(x, sep = "_")
}

parse_triggers <- function(x){
  x[is.na(x) | !nzchar(x)] <- "any"
  assert_legal_triggers(x)
  x
}
