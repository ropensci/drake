drake_split <- function(
  .data,
  # this is here so you can use the expected rows, if data does not yet exist
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
  splits_vec <- stringr::str_pad(
    seq(splits),
    pad = "0",
    side = "left",
    width = digits
    )
  # create a frame to create the slice_list
  slicelist_target <- paste("split", name, "indices", sep = "_")
  slicelist_command <- paste0("split_list(", name, ", splits = ", splits, ")")
  slicelist_plan <- data.frame(
    target = slicelist_target,
    command = slicelist_command
    )
  # deparse target names (pre-application)
  slice_targets <- paste(
    "split",
    name,
    splits_vec,
    sep = "_")
  slice_commands <- paste0(
    name, "[", slicelist_target, "[[", splits_vec, "]], ]"
    )
  # determine the number of rows in each split (also an integer)
  slice_plan <- data.frame(target = slice_targets, command = slice_commands)
  return(rbind(slicelist_plan, slice_plan))
}

split_list <- function(
  .data,
  splits
  ){
  # Initialize split_list
  split_list <- vector("list", length = splits)
  if (dplyr::is_grouped_df(.data)){
    # determine the size of each group
    data_groups <- dplyr::group_indices(.data)
  } else{
    ceil <- ceiling(nrow(.data) / splits)
    floo <- floor(nrow(.data) / splits)
    data_groups <- NULL
    for (i in seq(splits)){
      rep_num <- ifelse(
        (nrow(.data) - length(data_groups)) / floo == (splits - i + 1),
        floo,
        ceil
        )
      data_groups <- c(data_groups, rep(i, rep_num))
    }
  }
  stopifnot(length(data_groups) == nrow(.data))
  n_groups <- max(data_groups)
  for (i in seq(from = 1, to = n_groups)){
    push_to <- which.min(lapply(split_list, length))
    push_group <- names(which.max(table(data_groups)))
    push_indices <- which(data_groups == push_group)
    split_list[[push_to]] <- c(split_list[[push_to]], push_indices)
    # remove those indices from consideration
    data_groups[push_indices] <- NA
  }
  # ensure it's a numeric vector in each list element
  split_list <- lapply(split_list, as.integer)
  return(split_list)
}

drake_unsplit <- function(
  plan,
  out_target,
  last_method = gsub(
    x = tail(plan$target, 1),
    pattern = "^(.*)_split.*$", "\\1"
    ),
  plan_type = "analysis"
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
