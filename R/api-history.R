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
#' @param analyze Logical, whether to analyze [drake_plan()]
#'   commands for named atomic arguments to function calls.
#'   The values of these arguments will show up in the return value.
#'   Could be slow because this requires parsing and analyzing
#'   lots of R code.
#' @inheritParams outdated
#' @inheritParams drake_config
#' @examples
#' \dontrun{
#' isolate_example({
#' # First, let's iterate on a drake workflow.
#' load_mtcars_example()
#' make(my_plan, history = TRUE, verbose = 0L)
#' # Naturally, we'll make updates to our targets along the way.
#' reg2 <- function(d) {
#'   d$x2 <- d$x ^ 3
#'   lm(y ~ x2, data = d)
#' }
#' Sys.sleep(0.01)
#' make(my_plan, history = TRUE, verbose = 0L)
#' # The history is a data frame about all the recorded runs of your targets.
#' out <- drake_history(analyze = TRUE)
#' print(out)
#' # View(out)
#' # Let's use the history to recover the oldest version
#' # of our regression2_small target.
#' oldest_reg2_small <- max(which(out$target == "regression2_small"))
#' hash_oldest_reg2_small <- out[oldest_reg2_small, ]$hash
#' cache <- drake_cache()
#' cache$get_value(hash_oldest_reg2_small)
#' # If you run clean(), drake can still find all the targets.
#' clean(small)
#' drake_history()
#' # But if you run clean() with garbage collection,
#' # older versions of your targets may be gone.
#' clean(large, garbage_collection = TRUE)
#' drake_history()
#' })
#' }
drake_history <- function(
  cache = NULL,
  history = NULL,
  analyze = FALSE,
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
  from_cache <- lapply(from_txtq$message, history_from_cache, cache = cache)
  from_cache <- do.call(drake_bind_rows, from_cache)
  out <- merge(
    x = from_txtq,
    y = from_cache,
    by = "message",
    all.x = TRUE
  )
  out <- weak_tibble(
    target = out$title,
    time = out$time,
    hash = out$hash,
    exists = out$exists,
    command = out$command,
    runtime = out$runtime
  )
  out <- out[order(out$target, out$time), ]
  out$latest <- !duplicated(out$target, fromLast = TRUE)
  if (analyze) {
    args <- lapply(out$command, find_args)
    args <- do.call(drake_bind_rows, args)
    out <- cbind(out, args)
  }
  out$DRAKE_HISTORY_NA_ <- NULL
  weak_as_tibble(out)
}

history_from_cache <- function(meta_hash, cache) {
  if (!cache$exists_object(meta_hash)) {
    return(
      data.frame(
        message = meta_hash,
        hash = NA_character_,
        exists = NA,
        command = NA_character_,
        runtime = NA,
        stringsAsFactors = FALSE
      )
    )
  }
  meta <- cache$get_value(meta_hash)
  out <- data.frame(hash = meta$hash, stringsAsFactors = FALSE)
  out$exists <- cache$exists_object(meta$hash)
  out$command <- meta$command
  out$runtime <- meta$time_command$elapsed
  out$message <- meta_hash
  out
}

find_args <- function(command) {
  command <- parse(text = command)[[1]]
  ht <- ht_new()
  walk_args(command, ht)
  out <- as.data.frame(as.list(ht), stringsAsFactors = FALSE)
  if (!nrow(out)) {
    out <- data.frame(DRAKE_HISTORY_NA_ = NA, stringsAsFactors = FALSE)
  }
  out
}

walk_args <- function(expr, ht) {
  if (!length(expr)) {
    return()
  }
  if (is.call(expr)) {
    for (name in Filter(nzchar, names(expr))) {
      value <- expr[[name]]
      if (is.atomic(value)) {
        ht_set(ht = ht, x = name, value = value)
      }
    }
  }
  if (is.recursive(expr)) {
    lapply(expr, walk_args, ht = ht)
  }
}

default_history_queue <- function(cache_path) {
  assert_pkg("txtq", version = "0.1.2")
  cache_dir <- dirname(cache_path)
  history_path <- file.path(cache_dir, ".drake_history")
  txtq::txtq(history_path)
}

has_history <- function(config) {
  inherits(config$history, "R6_txtq")
}
