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
  plan$target <- make.names(plan$target, unique = FALSE, allow_ = TRUE)
  plan <- plan[nzchar(plan$target), ]
  first <- c("target", "command")
  cols <- c(first, setdiff(colnames(plan), first))
  if (!allow_duplicated_targets) {
    plan <- assert_unique_targets(plan[, cols])
  }
  plan <- arrange_plan_cols(plan)
  for (col in lang_cols(plan)) {
    if (!is.list(plan[[col]])) {
      plan[[col]] <- lapply(plan[[col]], safe_parse)
    }
  }
  as_drake_plan(plan)
}

sanitize_targets <- function(targets, plan) {
  if (is.null(try(targets, silent = TRUE))) {
    return(plan$target)
  }
  targets <- make.names(targets, unique = FALSE, allow_ = TRUE)
  not_found <- setdiff(targets, plan$target)
  if (length(not_found)) {
    warning(
      "ignoring targets not in the drake plan:\n",
      multiline_message(not_found),
      call. = FALSE
    )
  }
  unique(intersect(targets, plan$target))
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
