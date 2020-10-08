#' @title History and provenance
#' `r lifecycle::badge("stable")`
#' @description See the history and provenance of your targets:
#'   what you ran, when you ran it, the function arguments
#'   you used, and how to get old data back.
#' @details [drake_history()] returns a data frame with the following columns.
#'
#' - `target`: the name of the target.
#' - `current`: logical, whether the row describes the data
#'   actually assigned to the target name in the cache,
#'   e.g. what you get with `loadd(target)` and `readd(target)`.
#'   Does **NOT** tell you if the target is up to date.
#' - `built`: when the target's value was stored in the cache.
#'   This is the true creation date of the target's value,
#'   not the recovery date from `make(recover = TRUE)`.
#' - `exists`: logical, whether the target's historical value
#'   still exists in the cache. Garbage collection via
#'   (`clean(garbage_collection = TRUE)` and `drake_cache()$gc()`)
#'   remove these historical values, but `clean()` under the default
#'   settings does not.
#' - `hash`: fingerprint of the target's historical value in the cache.
#'   If the value still exists, you can read it with
#'   `drake_cache()$get_value(hash)`.
#' - `command`: the [drake_plan()] command executed to build the target.
#' - `seed`: random number generator seed.
#' - `runtime`: the time it took to execute the [drake_plan()] command.
#'   Does not include overhead due to `drake`'s processing.
#'
#' If `analyze` is `TRUE`, various other columns are included to show
#' the explicitly-named length-1 arguments to function calls in the commands.
#' See the "Provenance" section for more details.
#'
#' @section Provenance:
#' If `analyze` is `TRUE`, `drake`
#'   scans your [drake_plan()] commands
#'   for function arguments and mentions them in the history.
#'   A function argument shows up if and only if:
#'     1. It has length 1. \cr
#'     2. It is atomic, i.e. a base type: logical, integer,
#'        real, complex, character, or raw. \cr
#'     3. It is explicitly named in the function call,
#'        For example, `x` is detected as `1` in
#'        `fn(list(x = 1))` but not `f(list(1))`.
#'        The exceptions are [file_out()], [file_in()],
#'        and [knitr_in()]. For example, `filename` is detected
#'        as `"my_file.csv"` in
#'        `process_data(filename = file_in("my_file.csv"))`.
#'        NB: in `process_data(filename = file_in("a", "b"))`
#'        `filename` is not detected because the value must be atomic. \cr
#' @export
#' @return A data frame of target history.
#' @inheritParams drake_config
#' @param analyze Logical, whether to analyze [drake_plan()]
#'   commands for arguments to function calls.
#'   Could be slow because this requires parsing and analyzing
#'   lots of R code.
#' @param verbose Deprecated on 2019-09-11.
#' @examples
#' \dontrun{
#' isolate_example("contain side-effects", {
#' if (requireNamespace("knitr", quietly = TRUE)) {
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
#' invisible()
#' }
#' })
#' }
drake_history <- function(
  cache = NULL,
  history = NULL,
  analyze = TRUE,
  verbose = NULL
) {
  deprecate_verbose(verbose)
  if (is.null(cache)) {
    cache <- drake_cache()
  }
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  cache <- decorate_storr(cache)
  cache$set_history(history)
  if (is.null(cache$history)) {
    stop0("no history. Call make(history = TRUE) next time.")
  }
  from_txtq <- cache$history$list()
  from_cache <- lapply(from_txtq$message, history_from_cache, cache = cache)
  from_cache <- do.call(drake_bind_rows, from_cache)
  if (!nrow(from_txtq)) {
    stop0("no history. Call make(history = TRUE) next time.")
  }
  out <- merge(
    x = from_txtq,
    y = from_cache,
    by = "message",
    all.x = TRUE
  )
  out <- weak_tibble(
    target = out$title,
    built = out$time,
    hash = out$hash,
    exists = out$exists,
    command = out$command,
    runtime = out$runtime,
    seed = out$seed
  )
  out <- out[order(out$target, out$built), ]
  current_hash <- vapply(
    X = out$target,
    FUN = cache$safe_get_hash,
    FUN.VALUE = character(1),
    namespace = cache$default_namespace
  )
  out$current <- out$hash == current_hash
  out$current[is.na(out$current)] <- FALSE
  cols <- c(
    "target",
    "current",
    "built",
    "exists",
    "hash",
    "command",
    "seed",
    "runtime"
  )
  out <- out[, cols]
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
        built = NA_character_,
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
    fn <- safe_deparse(value[[1]], backtick = FALSE)
    if (fn %in% c(file_fns, no_deps_fns)) {
      value <- eval(value)
    }
  }
  if (is.atomic(value) && length(value) == 1) {
    ht_set(ht = ht, x = name, value = value)
  }
}
