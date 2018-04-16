sanitize_plan <- function(plan){
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
    plan$trigger[is.na(plan$trigger) | !nzchar(plan$trigger)] <- "any"
    assert_legal_triggers(plan[["trigger"]])
  }
  plan <- file_outs_to_targets(plan)
  plan$target <- repair_target_names(plan$target)
  plan <- plan[nzchar(plan$target), ]
  plan$command[is.na(plan$command)] <- ""
  first <- c("target", "command")
  cols <- c(first, setdiff(colnames(plan), first))
  handle_duplicated_targets(plan[, cols])
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
  x <- stringi::stri_trim_both(x)
  x[is_not_file(x)] <- gsub(illegals, "_", x[is_not_file(x)])
  x <- gsub("^_", "", x)
  x[!nzchar(x)] <- "X"
  make.unique(x, sep = "_")
}

file_outs_to_targets <- function(plan){
  index <- grepl("file_out", plan$command, fixed = TRUE)
  plan$target[index] <- vapply(
    plan$command[index],
    single_file_out,
    character(1)
  )
  plan$target[is_file(plan$target)] <-
    plan$target[is_file(plan$target)] %>%
    gsub(pattern = "^'|'$", replacement = "\"")
  plan
}

single_file_out <- function(command){
  file_out <- command_dependencies(command)$file_out
  if (length(file_out) < 1){
    stop("found an empty file_out() in command: ", command, call. = FALSE)
  }
  if (length(file_out) > 1){
    warning(
      "Multiple file outputs found for command `", command, "`. ",
      "Choosing ", file_out[1], " as the target name.",
      call. = FALSE
    )
    file_out[1]
  } else {
    file_out
  }
}
