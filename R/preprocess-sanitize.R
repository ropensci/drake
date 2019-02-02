sanitize_plan <- function(plan, allow_duplicated_targets = FALSE) {
  fields <- intersect(colnames(plan), c("command", "target", "trigger"))
  for (field in fields) {
    if (!is.null(plan[[field]])) {
      plan[[field]] <- factor_to_character(plan[[field]])
      if (is.character(plan[[field]])) {
        plan[[field]] <- trimws(plan[[field]])
      }
    }
  }
  plan$target <- repair_target_names(plan$target)
  plan <- plan[nzchar(plan$target), ]
  first <- c("target", "command")
  cols <- c(first, setdiff(colnames(plan), first))
  if (!allow_duplicated_targets) {
    plan <- assert_unique_targets(plan[, cols])
  }
  plan <- arrange_plan_cols(plan)
  for (col in lang_cols(plan)) {
    if (!is.list(plan[[col]])) {
      plan[[col]] <- lapply(plan[[col]], function(x) {
        parse(text = x, keep.source = FALSE)[[1]]
      })
    }
  }
  as_drake_plan(plan)
}

sanitize_targets <- function(plan, targets) {
  targets <- repair_target_names(targets)
  sanitize_nodes(nodes = targets, choices = plan$target)
}

sanitize_nodes <- function(nodes, choices) {
  if (!any(nodes %in% choices)) {
    stop(
      "All import/target names are invalid ",
      "in argument 'targets', 'from', or 'subset' ",
      "for make() or similar function.",
      call. = FALSE
    )
  }
  diffs <- setdiff(nodes, choices)
  if (length(diffs)) {
    warning(
      "Ignoring imports/targets that were requested but not found:\n",
      multiline_message(diffs),
      call. = FALSE
    )
  }
  unique(intersect(nodes, choices))
}

repair_target_names <- function(x) {
  make.names(x, unique = FALSE)
}

arrange_plan_cols <- function(plan) {
  primary <- c("target", "command")
  others <- setdiff(colnames(plan), primary)
  plan[, c(primary, others)]
}

assert_unique_targets <- function(plan) {
  dups <- duplicated(plan$target)
  if (any(dups)) {
    stop(
      "duplicated target names:\n",
      multiline_message(plan$target[dups]),
      call. = FALSE
    )
  }
  plan
}
