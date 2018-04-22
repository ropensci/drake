#' @title Generate a flat text log file
#'   to represent the state of the cache.
#' @description
#' This functionality is like
#' \code{\link{make}(..., cache_log_file = TRUE)},
#' but separated and more customizable.
#' The `drake_cache_log_file()` function writes a flat text file
#' to represents the state of all the targets and imports in the cache.
#' If you call it after each [make()]
#' and put the log file under version control,
#' you can track the changes to your results over time.
#' This way, your data is versioned alongside your code
#' in a easy-to-view format. Hopefully, this functionality
#' is a step toward better data versioning tools.
#' @seealso [drake_cache_log()],
#'   [make()],
#'   [get_cache()],
#'   [default_long_hash_algo()],
#'   [short_hash()],
#'   [long_hash()]
#' @export
#' @return There is no return value, but a log file is generated.
#' @param file character scalar, name of the flat text log file.
#'
#' @inheritParams cached
#'
#' @param jobs number of jobs/workers for parallel processing
#'
#' @param targets_only logical, whether to output information
#'   only on the targets in your workflow plan data frame.
#'   If `targets_only` is `FALSE`, the output will
#'   include the hashes of both targets and imports.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # Load drake's canonical example.
#' load_mtcars_example() # Get the code with drake_example()
#' # Run the project and save a flat text log file.
#' make(my_plan)
#' drake_cache_log_file() # writes drake_cache.log
#' # The above 2 lines are equivalent to make(my_plan, cache_log_file = TRUE) # nolint
#' # At this point, put drake_cache.log under version control
#' # (e.g. with 'git add drake_cache.log') alongside your code.
#' # Now, every time you run your project, your commit history
#' # of hash_lot.txt is a changelog of the project's results.
#' # It shows which targets and imports changed on every commit.
#' # It is extremely difficult to track your results this way
#' # by putting the raw '.drake/' cache itself under version control.
#' })
#' }
drake_cache_log_file <- function(
  file = "drake_cache.log",
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = drake::default_verbose(),
  jobs = 1,
  targets_only = FALSE
){
  if (!length(file) || identical(file, FALSE)){
    return(invisible())
  } else if (identical(file, TRUE)){
    file <- formals(drake_cache_log_file)$file
  }
  drake_cache_log(
    path = path,
    search = search,
    cache = cache,
    verbose = verbose,
    jobs = jobs,
    targets_only = targets_only
  ) %>% write.table(
    file = file,
    quote = FALSE,
    row.names = FALSE
  )
}

#' @title Get a table that represents the state of the cache.
#' @description
#' This functionality is like
#' \code{\link{make}(..., cache_log_file = TRUE)},
#' but separated and more customizable. Hopefully, this functionality
#' is a step toward better data versioning tools.
#' @details A hash is a fingerprint of an object's value.
#' Together, the hash keys of all your targets and imports
#' represent the state of your project.
#' Use `drake_cache_log()` to generate a data frame
#' with the hash keys of all the targets and imports
#' stored in your cache.
#' This function is particularly useful if you are
#' storing your drake project in a version control repository.
#' The cache has a lot of tiny files, so you should not put it
#' under version control. Instead, save the output
#' of `drake_cache_log()` as a text file after each [make()],
#' and put the text file under version control.
#' That way, you have a changelog of your project's results.
#' See the examples below for details.
#' Depending on your project's
#' history, the targets may be different than the ones
#' in your workflow plan data frame.
#' Also, the keys depend on the short hash algorithm
#' of your cache (default: [default_short_hash_algo()]).
#' @seealso [drake_cache_log_file()]
#'   [cached()],
#'   [get_cache()],
#'   [default_short_hash_algo()],
#'   [default_long_hash_algo()],
#'   [short_hash()],
#'   [long_hash()]
#' @export
#' @return Data frame of the hash keys of the targets and imports
#'   in the cache
#'
#' @inheritParams cached
#'
#' @param jobs number of jobs/workers for parallel processing
#'
#' @param targets_only logical, whether to output information
#'   only on the targets in your workflow plan data frame.
#'   If `targets_only` is `FALSE`, the output will
#'   include the hashes of both targets and imports.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # Load drake's canonical example.
#' load_mtcars_example() # Get the code with drake_example()
#' # Run the project, build all the targets.
#' make(my_plan)
#' # Get a data frame of all the hash keys.
#' # If you want a changelog, be sure to do this after every make().
#' cache_log <- drake_cache_log()
#' head(cache_log)
#' # Save the hash log as a flat text file.
#' write.table(
#'   x = cache_log,
#'   file = "drake_cache.log",
#'   quote = FALSE,
#'   row.names = FALSE
#' )
#' # At this point, put drake_cache.log under version control
#' # (e.g. with 'git add drake_cache.log') alongside your code.
#' # Now, every time you run your project, your commit history
#' # of hash_lot.txt is a changelog of the project's results.
#' # It shows which targets and imports changed on every commit.
#' # It is extremely difficult to track your results this way
#' # by putting the raw '.drake/' cache itself under version control.
#' })
#' }
drake_cache_log <- function(
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = drake::default_verbose(),
  jobs = 1,
  targets_only = FALSE
){
  if (is.null(cache)){
    return(
      tibble(
        hash = character(0),
        type = character(0),
        name = character(0)
      )
    )
  }
  out <- lightly_parallelize(
    X = cache$list(),
    FUN = single_cache_log,
    jobs = jobs,
    cache = cache
  ) %>%
    do.call(what = rbind) %>%
    as_tibble
  if (targets_only){
    out <- out[out$type == "target", ]
  }
  out
}

single_cache_log <- function(key, cache){
  hash <- cache$get_hash(key = key)
  imported <- get_from_subspace(
    key = key,
    subspace = "imported",
    namespace = "meta",
    cache = cache
  )
  imported <- ifelse(is.na(imported), TRUE, imported)
  type <- ifelse(imported, "import", "target")
  tibble(hash = hash, type = type, name = key)
}
