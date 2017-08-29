drake_split <- function(
  .data,
  out_target,
  # this is here so you can use the expected rows, if data does not yet exist
  data_rows = nrow(.data),
  splits = ceiling(nrow(.data) / 1e6)
  ){
  name <- deparse(substitute(.data))
  # Check if .data is an object in a magrittr pipeline.
  if (name == "."){
    name <- find_lhs_name(.data)
  }
  # Ensure that splits is an integer:
  splits <- ceiling(splits)
  digits <- ceiling(log10(splits))
  # deparse target names (pre-application)
  slice_targets <- paste(
    "slice",
    name,
    stringr::str_pad(seq(splits), pad = "0", side = "left", width = digits),
    sep = "_")
  # determine the number of rows in each split (also an integer)
  split_rows <- ceiling(data_rows / splits)
  if (dplyr::is_grouped_df(.data)){
    grouplist_target <- paste("slice", name, "grouplist", sep = "_")
    slice_targets <- c(
      grouplist_target,
      slice_targets
      )
    slice_commands <- c(
      paste0("group_list(", name, ", splits = ", splits, ")"),
      paste0(
        name,
        "[dplyr::group_indices(", name, ") %in% ",
        grouplist_target, "[[", seq(splits), "]], ]"
        )
      )
  } else {
    split_seq_low <- seq(
      from = 1L,
      by = split_rows,
      length.out = splits
      )
    split_seq_high <- seq(
      from = split_rows,
      by = split_rows,
      length.out = splits
      )
    slice_commands <- paste0(
      "dplyr::slice(",
      name, ", ",
      split_seq_low, ":", split_seq_high,
      ")")
  }
  slice_plan <- data.frame(target = slice_targets, command = slice_commands)
  return(slice_plan)
}

group_list <- function(
  .data,
  splits
  ){
  # determine the size of each group
  data_groups <- dplyr::group_size(.data)
  names(data_groups) <- seq_along(data_groups)
  # ensure split_rows is an integer
  split_rows <- ceiling(sum(data_groups) / splits)
  group_list <- vector("list", length = splits)
  for (i in data_groups){
    push_to <- which.min(lapply(group_list, sum))
    push_value <- data_groups[which.max(data_groups)]
    group_list[[push_to]] <- c(group_list[[push_to]], push_value)
    data_groups[which.max(data_groups)] <- NA
  }
  # Extract just the names, since those are the actual values we want.
  group_list <- lapply(group_list, names)
  group_list <- lapply(group_list, as.numeric)
  return(group_list)
}

drake_unsplit <- function(
  plan,
  out_target,
  last_method = gsub(
    x = tail(plan$target, 1),
    pattern = "^(.*)_slice.*$", "\\1"
    ),
  plan_type = "analysis" #c("analysis", "split")
  ){
  stopifnot(is.character(last_method))
  stopifnot(plan_type %in% c("analysis", "split"))
  if (missing(out_target)){
    out_target <- deparse(substitute(plan))
  }
  stopifnot(is.character(out_target))
  if (plan_type == "analysis"){
    bind_targets <- paste0(
      grep(
        pattern = last_method,
        x = plan$target,
        value = TRUE
        ),
      collapse = ", "
      )
  } else {
    bind_targets <- paste0(
      last_method, "_",
      split_plan[["target"]],
      collapse = ", "
      )
  }
  unsplit_plan <- data.frame(
    target = out_target,
    command = paste0("rbind(", bind_targets, ")")
    )
  return(unsplit_plan)
}

# So, so much thanks to Mr. Flick
# https://stackoverflow.com/a/42561430/7571303
# TODO: Make some unit tests for this
find_lhs_name <- function(x) {
  i <- 1
  while (
    !("chain_parts" %in% ls(envir = parent.frame(i)))
    && i < sys.nframe()
    ) {
    i <- i + 1
  }
  return(parent.frame(i)$lhs)
}
