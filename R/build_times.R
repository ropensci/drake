#' @title List the time it took to build each target/import.
#' @description Listed times do not include the amount of time
#' spent loading and saving objects!
#' @seealso [built()]
#' @export
#' @return A data frame of times, each from [system.time()].
#' @param targets_only logical, whether to only return the
#'   build times of the targets (exclude the imports).
#' @param path Root directory of the drake project,
#'   or if `search` is `TRUE`, either the
#'   project root or a subdirectory of the project.
#' @param search logical. If `TRUE`, search parent directories
#'   to find the nearest drake cache. Otherwise, look in the
#'   current working directory only.
#' @param digits How many digits to round the times to.
#' @param cache optional drake cache. If supplied,
#'   the `path` and `search` arguments are ignored.
#' @param verbose whether to print console messages
#' @param jobs number of parallel jobs/workers for light parallelism.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # Show the build times for the basic example.
#' load_basic_example() # Get the code with drake_example("basic").
#' make(my_plan) # Build all the targets.
#' build_times() # Show how long it took to build each target.
#' })
#' }
build_times <- function(
  path = getwd(),
  search = TRUE,
  digits = 3,
  cache = get_cache(path = path, search = search, verbose = verbose),
  targets_only = FALSE,
  verbose = TRUE,
  jobs = 1
){
  if (is.null(cache)){
    return(empty_times())
  }
  out <- lightly_parallelize(
    X = cache$list(namespace = "meta"),
    FUN = fetch_runtime,
    jobs = 1,
    cache = cache
  ) %>%
    parallel_filter(f = is.data.frame, jobs = jobs) %>%
    do.call(what = rbind) %>%
    rbind(empty_times()) %>%
    round_times(digits = digits) %>%
    to_build_duration_df
  out <- out[order(out$item), ]
  out$type[is.na(out$type)] <- "target"
  if (targets_only){
    out <- out[out$type == "target", ]
  }
  out
}

fetch_runtime <- function(key, cache){
  x <- get_from_subspace(
    key = key,
    subspace = "build_times",
    namespace = "meta",
    cache = cache
  )
  if (is_bad_time(x)){
    return(empty_times())
  }
  if (class(x) == "proc_time"){
    x <- runtime_entry(runtime = x, target = key, imported = NA)
  }
  x
}

empty_times <- function(){
  data.frame(
    item = character(0),
    type = character(0),
    elapsed = numeric(0),
    user = numeric(0),
    system = numeric(0),
    stringsAsFactors = FALSE
  )
}

round_times <- function(times, digits){
  for (col in time_columns){
    times[[col]] <- round(times[[col]], digits = digits)
  }
  times
}

runtime_entry <- function(runtime, target, imported){
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

to_build_duration_df <- function(times){
  eval(parse(text = "require(methods, quietly = TRUE)")) # needed for lubridate
  for (col in time_columns){
    times[[col]] <- to_build_duration(times[[col]])
  }
  times
}

# From lubridate issue 472,
# we need to round to the nearest second
# for times longer than a minute.
to_build_duration <- function(x){
  round_these <- x >= 60
  x[round_these] <- round(x[round_these], digits = 0)
  dseconds(x)
}

time_columns <- c("elapsed", "user", "system")

finalize_times <- function(target, meta, config){
  if (!is_bad_time(meta$command_time)){
    meta$command_time <- runtime_entry(
      runtime = meta$command_time,
      target = target,
      imported = meta$imported
    )
  }
  if (!is_bad_time(meta$start)){
    meta$build_time <- runtime_entry(
      runtime = proc.time() - meta$start,
      target = target,
      imported = meta$imported
    )
  }
  meta
}

is_bad_time <- function(x){
  !length(x) || is.na(x[1])
}
