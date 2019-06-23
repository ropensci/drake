#' @title Show history
#' @description See the history of your targets:
#'   what you ran, when you ran it, and where
#'   it lives. Optionally show the settings
#'   under which you ran things (could be slow)
#'   and whether each target is up to date (also could be slow).
#' @export
#' @return A data frame of target history.
#' @param cache An optional
#'   `drake`/[`storr`](https://github.com/richfitz/storr) cache.
#'   If not supplied, `drake` will try to find the default cache:
#'   a folder called `.drake/` in the current working directory
#'   or one of its ancestor directories.
#' @param history An optional [`txtq`](https://github.com/wlandau/txtq)
#'   produced by `make(history = TRUE)`. If not supplied,
#'   `drake` will try to find it next to the cache in a folder
#'   called `.drake_history/`.
#' @param find_args Logical, whether to search for atomic arguments
#'   to function calls inside [drake_plan()] commands.
#'   Could be slow because this requires parsing and analyzing
#'   lots of R code.
#' @inheritParams outdated
#' @inheritParams drake_config
drake_history <- function(
  cache = NULL,
  history = NULL,
  find_args = FALSE,
  make_imports = TRUE,
  do_prework = TRUE,
  verbose = TRUE
) {
  if (is.null(cache)) {
    cache <- drake_cache(verbose = verbose)
  }
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  if (is.null(history)) {
    history <- default_history_queue(force_cache_path(cache))
  }
  from_txtq <- history$list()
  precedence <- with(from_txtq, order(title, rev(time)))
  from_txtq <- from_txtq[precedence, ]
  from_txtq$latest <- !duplicated(from_txtq$title)
  from_cache <- lapply(from_txtq$message, history_from_cache, cache = cache)
  from_cache <- do.call(drake_bind_rows, from_cache)
  out <- weak_tibble(
    target = from_txtq$title,
    time = from_txtq$time,
    hash = from_cache$hash,
    exists = from_cache$exists,
    latest = from_txtq$latest,
    command = from_cache$command,
    runtime = from_cache$runtime
  )
  if (find_args) {
    args <- lapply(out$command, find_args)
    args <- do.call(drake_bind_rows, args)
    out <- cbind(out, args)
  }
  out
}

history_from_cache <- function(meta_hash, cache) {
  if (!cache$exists_object(meta_hash)) {
    return()
  }
  meta <- cache$get_value(meta_hash)
  out <- data.frame(hash = meta$hash)
  out$exists = cache$exists_object(meta$hash)
  out$command <- meta$command
  out$runtime <- meta$time_command$elapsed
  out
}

find_args <- function(command) {
  command <- parse(text = command)[[1]]

  browser()
}

default_history_queue <- function(cache_path) {
  assert_pkg("txtq", version = "0.1.2")
  cache_dir <- dirname(cache_path)
  history_path <- file.path(cache_dir, ".drake_history")
  txtq::txtq(history_path)
}

has_history <- function(config) {
  if (is.null(config$history)) {
    return(FALSE)
  }
  inherits(config$history, "R6_txtq")
}
