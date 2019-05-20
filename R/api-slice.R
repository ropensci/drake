#' @title Take a strategic subset of a dataset.
drake_slice <- function(data, margin, splits, index, drop = FALSE) {
  sclr <- length(margin) == 1L && length(splits) == 1L && length(index) == 1L
  if (!sclr) {
    stop(
      "In drake_slice, arguments margin, splits, ",
      "and index must each have length 1.",
      call. = FALSE
    )
  }
  args <- list(data)
  dim <- dim(data) %||% length(data)
  for (m in seq_along(dim)) {
    if (m == margin) {
      args[[m + 1]] <- slice_indices(dim[m], splits, index)
    } else {
      args[[m + 1]] <- substitute()
    }
  }
  args$drop <- drop
  do.call(`[`, args)
}

slice_indices <- function(length, splits, index) {
  if (length < 1L || splits < 1L || index < 1L || index > splits) {
    return(integer(0))
  }
  delta <- as.integer(length / splits)
  seq(from = 1L + delta * (index - 1L), to = delta * index, by = 1L)
}
