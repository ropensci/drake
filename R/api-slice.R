#' @title Take a strategic subset of a dataset.
#' @description `drake_slice()` is similar to `split()`.
#'   Both functions partition data into disjoint subsets,
#'   but whereas `split()` returns *all* the subsets, `drake_slice()`
#'   returns just *one*. In other words, `drake_slice(..., index = i)`
#'   returns `split(...)[[i]]`.
#'   Other features:
#'     1. `drake_slice()` works on vectors, data frames,
#'        matrices, lists, and arbitrary arrays.
#'     2. Like `parallel::splitIndices()`, `drake_slice()` tries to
#'        distribute the data uniformly across subsets.
#' See the examples to learn why splitting is useful in `drake`.
#' @export
#' @return A subset of `data`.
#' @param data A list, vector, data frame, matrix, or arbitrary array.
#'   Anything with a `length()` or `dim()`.
#' @param slices Integer of length 1, number of slices (i.e. pieces)
#'   of the whole dataset. Remember, `drake_slice(index = i)` returns
#'   only slice number `i`.
#' @param index Integer of length 1, which piece of the partition to return.
#' @param margin Integer of length 1, margin over which to split the data.
#'   For example, for a data frame or matrix,
#'   use `margin = 1` to split over rows and `margin = 2`
#'   to split over columns. Similar to `MARGIN` in `apply()`.
#' @param drop Logical, for matrices and arrays.
#'   If `TRUE`,` the result is coerced to the lowest possible dimension.
#'   See ?`[` for details.
#' @examples
#' # Simple usage
#' x <- matrix(seq_len(20), nrow = 5)
#' x
#' drake_slice(x, slices = 3, index = 1)
#' drake_slice(x, slices = 3, index = 2)
#' drake_slice(x, slices = 3, index = 3)
#' drake_slice(x, slices = 3, margin = 2, index = 1)
#' # In drake, you can split a large dataset over multiple targets.
#' \dontrun{
#' plan <- drake_plan(
#'   large_data = iris,
#'   data_split = target(
#'     drake_slice(large_data, slices = 50, index = i),
#'     transform = map(i = !!seq_len(50))
#'   )
#' )
#' plan
#' cache <- storr::storr_environment()
#' make(plan, cache = cache, session_info = FALSE, verbose = FALSE)
#' readd(data_split_1L, cache = cache)
#' readd(data_split_2L, cache = cache)
#' }
drake_slice <- function(data, slices, index, margin = 1L, drop = FALSE) {
  check_drake_slice_args(margin, slices, index)
  args <- list(data)
  dim <- dim(data) %||% length(data)
  margin <- ifelse(is.null(dim(data)), 1L, margin)
  for (m in seq_along(dim)) {
    if (m == margin) {
      args[[m + 1]] <- slice_indices(dim[m], slices, index)
    } else {
      args[[m + 1]] <- substitute()
    }
  }
  args$drop <- drop
  do.call(`[`, args)
}

slice_indices <- function(length, slices, index) {
  if (length < 1L || slices < 1L || index < 1L || index > slices) {
    return(integer(0))
  }
  inc <- as.integer(length / slices)
  mod <- length %% slices
  n <- inc + as.integer(index <= mod)
  from <- 1L + inc * (index - 1L) + min(index - 1L, mod)
  seq(from = from, length.out = n)
}

check_drake_slice_args <- function(slices, index, margin) {
  sclr <- length(slices) == 1L && length(index) && length(margin) == 1L
  if (sclr) {
    return()
  }
  stop(
    "In drake_slice, arguments margin, slices, ",
    "and index must each have length 1.",
    call. = FALSE
  )
}
