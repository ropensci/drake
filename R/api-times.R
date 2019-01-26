#' @title List the time it took to build each target.
#' @description Applies to targets in your plan, not imports or files.
#' @seealso [predict_runtime()]
#' @export
#' @return A data frame of times, each from [system.time()].
#' @inheritParams cached
#' @param ... Targets to load from the cache: as names (symbols) or
#'   character strings. If the `tidyselect` package is installed,
#'   you can also supply `dplyr`-style `tidyselect`
#'   commands such as `starts_with()`, `ends_with()`, and `one_of()`.
#' @param targets_only Deprecated.
#' @param digits How many digits to round the times to.
#' @param type Type of time you want: either `"build"`
#'   for the full build time including the time it took to
#'   store the target, or `"command"` for the time it took
#'   just to run the command.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # Show the build times for the mtcars example.
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Build all the targets.
#' if (requireNamespace("lubridate")) {
#'   print(build_times()) # Show how long it took to build each target.
#' }
#' })
#' }
build_times <- function(
  ...,
  path = getwd(),
  search = TRUE,
  digits = 3,
  cache = get_cache(path = path, search = search, verbose = verbose),
  targets_only = NULL,
  verbose = 1L,
  jobs = 1,
  type = c("build", "command")
) {
  deprecate_targets_only(targets_only) # 2019-01-03 # nolint
  if (is.null(cache)) {
    return(weak_as_tibble(empty_times()))
  }
  eval(parse(text = "require(methods, quietly = TRUE)")) # needed for lubridate
  targets <- as.character(match.call(expand.dots = FALSE)$...)
  if (exists_tidyselect()) {
    targets <- drake_tidyselect(cache = cache, ..., namespaces = "meta")
  }
  if (!length(targets)) {
    targets <- parallel_filter(
      x = cache$list(namespace = "meta"),
      f = function(target) {
        !is_imported_cache(target = target, cache = cache) &&
        !is_encoded_path(target)
      },
      jobs = jobs
    )
  }
  if (!length(targets)) {
    return(weak_as_tibble(empty_times()))
  }
  type <- match.arg(type)
  out <- lightly_parallelize(
    X = targets,
    FUN = fetch_runtime,
    jobs = 1,
    cache = cache,
    type = type
  )
  out <- parallel_filter(out, f = is.data.frame, jobs = jobs)
  out <- do.call(rbind, out)
  out <- rbind(out, empty_times())
  out <- round_times(out, digits = digits)
  out <- to_build_duration_df(out)
  out <- out[order(out$target), ]
  tryCatch(
    weak_as_tibble(out),
    error = error_tibble_times
  )
}

fetch_runtime <- function(key, cache, type) {
  x <- get_from_subspace(
    key = key,
    subspace = paste0("time_", type),
    namespace = "meta",
    cache = cache
  )
  if (is_bad_time(x)) {
    x <- empty_times()
  } else if (inherits(x, "proc_time")) {
    x <- runtime_entry(runtime = x, target = key)
  }
  weak_as_tibble(x)
}

empty_times <- function() {
  list(
    target = character(0),
    elapsed = numeric(0),
    user = numeric(0),
    system = numeric(0)
  )
}

round_times <- function(times, digits) {
  for (col in time_columns) {
    if (length(times[[col]])) {
      times[[col]] <- round(times[[col]], digits = digits)
    }
  }
  times
}

runtime_entry <- function(runtime, target) {
  list(
    target = target,
    elapsed = runtime[["elapsed"]],
    user = runtime[["user.self"]],
    system = runtime[["sys.self"]]
  )
}

to_build_duration_df <- function(times) {
  eval(parse(text = "require(methods, quietly = TRUE)")) # needed for lubridate
  for (col in time_columns) {
    if (length(times[[col]])) {
      times[[col]] <- to_build_duration(times[[col]])
    }
  }
  times
}

# From lubridate issue 472,
# we need to round to the nearest second
# for times longer than a minute.
to_build_duration <- function(x) {
  assert_pkg("lubridate")
  round_these <- x >= 60
  x[round_these] <- round(x[round_these], digits = 0)
  lubridate::dseconds(x)
}

time_columns <- c("elapsed", "user", "system")

finalize_times <- function(target, meta, config) {
  meta$time_command <- runtime_entry(
    runtime = meta$time_command,
    target = target
  )
  meta$time_build <- runtime_entry(
    runtime = proc.time() - meta$time_start,
    target = target
  )
  meta
}

is_bad_time <- function(x) {
  !length(x) || is.na(x[1])
}
