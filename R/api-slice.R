#' @title Take a strategic subset of a dataset.
#' @description Split a list, data frame, matrix, or other
drake_slice <- function(data, splits, index, margin = 1L, drop = FALSE) {
  check_drake_slice_args(margin, splits, index)
  args <- list(data)
  dim <- dim(data) %||% length(data)
  margin <- ifelse(is.null(dim(data)), 1L, margin)
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
  inc <- as.integer(length / splits)
  mod <- length %% splits
  n <- inc + as.integer(index <= mod)
  from <- 1L + inc * (index - 1L) + min(index - 1L, mod)
  seq(from = from, length.out = n)
}

check_drake_slice_args <- function(splits, index, margin) {
  sclr <- length(splits) == 1L && length(index) && length(margin) == 1L
  if (sclr) {
    return()
  }
  stop(
    "In drake_slice, arguments margin, splits, ",
    "and index must each have length 1.",
    call. = FALSE
  )
}
