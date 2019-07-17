#' @title History and provenance
#' @description See the history and provenance of your targets:
#'   what you ran, when you ran it, the function arguments
#'   you used, and how to get old data back.
#' @details If `analyze` is `TRUE`, `drake`
#'   scans your [drake_plan()] commands
#'   for function arguments and mentions them in the history.
#'   A function argument shows up if and only if
#'     1. It has length 1.
#'     2. It is atomic, i.e. a base type: logical, integer,
#'        real, complex, character, or raw.
#'     3. It is explicitly named in the function call,
#'        For example, `x` is detected as `1` in
#'        `fn(list(x = 1))` but not `f(list(1))`.
#'        The exceptions are [file_out()], [file_in()],
#'        and [knitr_in()]. For example, `filename` is detected
#'        as `"my_file.csv"` in
#'        `process_data(filename = file_in("my_file.csv"))`.
#'        NB: in `process_data(filename = file_in("a", "b"))`
#'        `filename` is not detected because the value must be atomic.
#' @export
#' @return A data frame of target history.
#' @param analyze Logical, whether to analyze [drake_plan()]
#'   commands for arguments to function calls.
#'   Could be slow because this requires parsing and analyzing
#'   lots of R code.
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
#' # Let's use the history to recover the oldest version
#' # of our regression2_small target.
#' oldest_reg2_smsall <- max(which(out$target == "regression2_small"))
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
  analyze = TRUE,
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
  if (!nrow(from_txtq)) {
    stop("no history. Call make(history = TRUE) next time.", call. = FALSE)
  }
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
    runtime = out$runtime,
    seed = out$seed
  )
  out <- out[order(out$target, out$time), ]
  out$latest <- !duplicated(out$target, fromLast = TRUE)
  if (analyze) {
    args <- lapply(out$command, history_find_args)
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
        seed = NA_integer_,
        stringsAsFactors = FALSE
      )
    )
  }
  meta <- cache$get_value(meta_hash)
  out <- data.frame(hash = meta$hash, stringsAsFactors = FALSE)
  out$exists <- cache$exists_object(meta$hash)
  out$command <- meta$command
  out$runtime <- meta$time_command$elapsed
  out$seed <- meta$seed
  out$message <- meta_hash
  out
}

history_find_args <- function(command) {
  command <- parse(text = command)[[1]]
  ht <- ht_new()
  history_walk_args(command, ht)
  out <- as.data.frame(as.list(ht), stringsAsFactors = FALSE)
  if (!nrow(out)) {
    out <- data.frame(DRAKE_HISTORY_NA_ = NA, stringsAsFactors = FALSE)
  }
  out
}

history_walk_args <- function(expr, ht) {
  if (!length(expr)) {
    return()
  }
  if (is.call(expr)) {
    for (name in Filter(nzchar, names(expr))) {
      history_analyze_value(name = name, value = expr[[name]], ht = ht)
    }
  }
  if (is.recursive(expr)) {
    lapply(expr, history_walk_args, ht = ht)
  }
}

history_analyze_value <- function(name, value, ht) {
  if (is.call(value)) {
    fn <- safe_deparse(value[[1]])
    if (fn %in% c(file_fns, no_deps_fns)) {
      value <- eval(value)
    }
  }
  if (is.atomic(value) && length(value) == 1) {
    ht_set(ht = ht, x = name, value = value)
  }
}

default_history_queue <- function(cache_path) {
  cache_dir <- dirname(cache_path)
  history_path <- file.path(cache_dir, ".drake_history")
  txtq::txtq(history_path)
}

is_history <- function(history) {
  inherits(history, "R6_txtq")
}

initialize_history <- function(history, cache_path) {
  if (identical(history, TRUE)) {
    history <- default_history_queue(cache_path)
  }
  if (!is.null(history)) {
    stopifnot(is_history(history))
  }
  history
}
