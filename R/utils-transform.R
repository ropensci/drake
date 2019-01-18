trf_aggregate <- function(plan, command, groups) {
  for (group in groups) {
    levels <- unique(plan[[group]])
    levels <- paste(levels, levels, sep = " = ")
    levels <- paste(levels, collapse = ", ")
    command <- gsub(group, levels, command, fixed = TRUE)
  }
  data.frame(command = command, stringsAsFactors = FALSE)
}

trf_check_conflicts <- function(plan, cols) {
  x <- intersect(attr(plan, "protect"), cols)
  if (length(x)) {
    stop(
      "variables in `target(transform = ...)` ",
      "cannot also be custom column names in the plan:\n",
      multiline_message(x),
      call. = FALSE
    )
  }
}

trf_cols <- function(plan) {
  setdiff(colnames(plan), attr(plan, "protect"))
}

trf_grid <- function(plan, levels) {
  args <- c(levels, stringsAsFactors = FALSE)
  grid <- do.call(what = expand.grid, args = args)
  if (length(trf_cols(plan))) {
    grid <- merge(grid, plan[, trf_cols(plan)])
  }
  grid
}

trf_levels <- function(plan, transform) {
  transform <- transform[-1]
  names <- names(transform) %||% rep("", length(transform))
  out <- lapply(transform[nzchar(names)], function(x) {
    as.character(x)[-1]
  })
  planned <- vapply(
    transform[!nzchar(names)],
    as.character,
    FUN.VALUE = character(1)
  )
  for (factor in planned) {
    out[[factor]] <- as.character(na_omit(plan[[factor]]))
  }
  out
}

trf_parse_custom_groups <- function(plan, row) {
  if (!("group" %in% colnames(plan))) {
    return(character(0))
  }
  groups <- plan$group[[row]]
  if (is.character(groups)) {
    groups <- parse(text = groups)
  }
  all.vars(groups)
}
