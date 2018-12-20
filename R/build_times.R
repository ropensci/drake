#' @title List the time it took to build each target/import.
#' @description Listed times do not include the amount of time
#'  spent loading and saving objects! See the `type`
#'  argument for different versions of the build time.
#'  (You can choose whether to take storage time into account.)
#' @seealso [built()]
#' @export
#' @return A data frame of times, each from [system.time()].
#' @inheritParams cached
#' @param ... targets to load from the cache: as names (symbols) or
#'   character strings. If the `tidyselect` package is installed,
#'   you can also supply `dplyr`-style `tidyselect`
#'   commands such as `starts_with()`, `ends_with()`, and `one_of()`.
#' @param targets_only logical, whether to only return the
#'   build times of the targets (exclude the imports).
#' @param digits How many digits to round the times to.
#' @param type Type of time you want: either `"build"`
#'   for the full build time including the time it took to
#'   store the target, or `"command"` for the time it took
#'   just to run the command.
#' @param pretty_files logical. If `TRUE`, files are displayed
#'   in the `item` column as "file my_file.Rmd". If `FALSE`,
#'   files are encoded according to `drake`'s internal standard
#'   for representing file paths, which may not be human readable
#'   in future versions of `drake`. 
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # Show the build times for the mtcars example.
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Build all the targets.
#' build_times() # Show how long it took to build each target.
#' })
#' }
build_times <- function(
  ...,
  path = getwd(),
  search = TRUE,
  digits = 3,
  cache = get_cache(path = path, search = search, verbose = verbose),
  targets_only = FALSE,
  verbose = drake::default_verbose(),
  jobs = 1,
  type = c("build", "command"),
  pretty_files = TRUE
) {
  eval(parse(text = "require(methods, quietly = TRUE)")) # needed for lubridate
  if (is.null(cache)) {
    return(empty_times())
  }
  targets <- as.character(match.call(expand.dots = FALSE)$...)
  if (exists_tidyselect()) {
    targets <- drake_tidyselect(cache = cache, ..., namespaces = "meta")
  }
  if (!length(targets)) {
    targets <- cache$list(namespace = "meta")
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
  out <- out[order(out$item), ]
  out$type[is.na(out$type)] <- "target"
  if (targets_only) {
    out <- out[out$type == "target", ]
  }
  if (pretty_files) {
    out$item <- display_path(out$item)
  }
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
    return(empty_times())
  }
  if (inherits(x, "proc_time")) {
    x <- runtime_entry(runtime = x, target = key, imported = NA)
  }
  x
}

empty_times <- function() {
  data.frame(
    item = character(0),
    type = character(0),
    elapsed = numeric(0),
    user = numeric(0),
    system = numeric(0),
    stringsAsFactors = FALSE
  )
}

round_times <- function(times, digits) {
  for (col in time_columns) {
    times[[col]] <- round(times[[col]], digits = digits)
  }
  times
}

runtime_entry <- function(runtime, target, imported) {
  type <- ifelse(imported, "import", "target")
  data.frame(
    item = target,
    type = type,
    elapsed = runtime[["elapsed"]],
    user = runtime[["user.self"]],
    system = runtime[["sys.self"]],
    stringsAsFactors = FALSE
  )
}

to_build_duration_df <- function(times) {
  eval(parse(text = "require(methods, quietly = TRUE)")) # needed for lubridate
  for (col in time_columns) {
    times[[col]] <- to_build_duration(times[[col]])
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
  if (!is_bad_time(meta$time_command)) {
    meta$time_command <- runtime_entry(
      runtime = meta$time_command,
      target = target,
      imported = meta$imported
    )
  }
  if (!is_bad_time(meta$start)) {
    meta$time_build <- runtime_entry(
      runtime = proc.time() - meta$start,
      target = target,
      imported = meta$imported
    )
  }
  meta
}

is_bad_time <- function(x) {
  !length(x) || is.na(x[1])
}
