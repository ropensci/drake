#' @title See the time it took to build each target.
#' `r lifecycle::badge("stable")`
#' @description Applies to targets in your plan, not imports or files.
#' @details Times for dynamic targets
#'   (`https://books.ropensci.org/drake/dynamic.html`)
#'   only reflect the time it takes
#'   to post-process the sub-targets (typically very fast)
#'   and exclude the time it takes to build the sub-targets themselves.
#'   Sub-targets build times are listed individually.
#' @seealso [predict_runtime()]
#' @export
#' @return A data frame of times, each from [system.time()].
#' @inheritParams cached
#' @param ... Targets to load from the cache: as names (symbols) or
#'   character strings. If the `tidyselect` package is installed,
#'   you can also supply `dplyr`-style `tidyselect`
#'   commands such as `starts_with()`, `ends_with()`, and `one_of()`.
#' @param list Character vector of targets to select.
#' @param targets_only Deprecated.
#' @param digits How many digits to round the times to.
#' @param type Type of time you want: either `"build"`
#'   for the full build time including the time it took to
#'   store the target, or `"command"` for the time it took
#'   just to run the command.
#' @param verbose Deprecated on 2019-09-11.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' if (requireNamespace("lubridate")) {
#' # Show the build times for the mtcars example.
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Build all the targets.
#' print(build_times()) # Show how long it took to build each target.
#' }
#' }
#' })
#' }
build_times <- function(
  ...,
  path = NULL,
  search = NULL,
  digits = 3,
  cache = drake::drake_cache(path = path),
  targets_only = NULL,
  verbose = NULL,
  jobs = 1,
  type = c("build", "command"),
  list = character(0)
) {
  deprecate_verbose(verbose)
  deprecate_search(search)
  deprecate_targets_only(targets_only) # 2019-01-03 # nolint
  assert_pkg("lubridate")
  if (is.null(cache)) {
    return(weak_as_tibble(empty_times()))
  }
  cache <- decorate_storr(cache)
  eval(parse(text = "require(methods, quietly = TRUE)")) # needed for lubridate
  targets <- c(as.character(match.call(expand.dots = FALSE)$...), list)
  if (requireNamespace("tidyselect", quietly = TRUE)) {
    targets <- drake_tidyselect_cache(
      ...,
      list = list,
      cache = cache,
      namespaces = "meta"
    )
  }
  if (!length(targets)) {
    targets <- cache$list(namespace = "meta")
  }
  if (!length(targets)) {
    return(weak_as_tibble(empty_times()))
  }
  type <- match.arg(type)
  type <- paste0("time_", type)
  meta <- cache$mget(key = targets, namespace = "meta")
  keep <- vapply(meta, keep_build_time, FUN.VALUE = logical(1))
  meta <- meta[keep]
  times <- lapply(meta, extract_runtime, type = type)
  out <- list()
  out$target <- as.character(unlist(lapply(times, `[[`, x = "target")))
  dseconds <- lubridate::dseconds
  for (time in c("elapsed", "user", "system")) {
    x <- as.numeric(unlist(lapply(times, `[[`, x = time)))
    x <- round(x, digits = 3)
    out[[time]] <- to_build_duration(x, dseconds = dseconds)
  }
  weak_as_tibble(out)
}

keep_build_time <- function(meta) {
  !(meta$imported %|||% FALSE) &&
    !(meta$isfile %|||% FALSE) &&
    !is.null(meta$time_build)
}

error_tibble_times <- function(e) {
  stop0(
    "Cannot convert a data frame of times to a tibble. ",
    "Install pillar version 1.2.1 or greater."
  )
}

# From lubridate issue 472,
# we need to round to the nearest second
# for times longer than a minute.
to_build_duration <- function(x, dseconds) {
  round_these <- x >= 60
  x[round_these] <- round(x[round_these], digits = 0)
  dseconds(x)
}

extract_runtime <- function(meta, type) {
  x <- meta[[type]]
  if (is_bad_time(x)) {
    x <- empty_times()
  } else if (inherits(x, "proc_time")) {
    x <- runtime_entry(runtime = x, target = meta$target)
  }
  x
}

is_bad_time <- function(x) {
  is.null(x) || is.na(x[1])
}

empty_times <- function() {
  list(
    target = character(0),
    elapsed = numeric(0),
    user = numeric(0),
    system = numeric(0)
  )
}
