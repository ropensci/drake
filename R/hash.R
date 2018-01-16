#' @title List the available hash algorithms for drake caches.
#' @export
#' @description See the advanced storage tutorial
#' at \url{https://wlandau-lilly.github.io/drake/articles/storage.html}
#' for details.
#' @return A character vector of names of available hash algorithms.
#' @examples
#' available_hash_algos()
available_hash_algos <- function(){
  eval(formals(digest::digest)$algo)
}

#' @title Get the long hash algorithm of a drake cache.
#' @export
#' @seealso \code{\link{default_short_hash_algo}},
#' \code{\link{default_long_hash_algo}}
#' @description  See the advanced storage tutorial
#' at \url{https://wlandau-lilly.github.io/drake/articles/storage.html}
#' for details.
#' @return A character vector naming a hash algorithm.
#' @param cache drake cache
#' @param verbose whether to print console messages
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load the canonical example for drake.
#' # Run the project and return the internal master configuration list.
#' config <- make(my_plan)
#' # Locate the storr cache.
#' cache <- config$cache
#' # Get the long hash algorithm of the cache.
#' long_hash(cache)
#' })
#' }
long_hash <- function(
  cache = drake::get_cache(verbose = verbose),
  verbose = TRUE
){
  if (!cache$exists(key = "long_hash_algo", namespace = "config")){
    return(NULL)
  }
  cache$get("long_hash_algo", namespace = "config")
}

#' @title Get the short hash algorithm of a drake cache.
#' @export
#' @seealso \code{\link{default_short_hash_algo}},
#' \code{\link{default_long_hash_algo}}
#' @description See the advanced storage tutorial
#' at \url{https://wlandau-lilly.github.io/drake/articles/storage.html}
#' for details.
#' @return A character vector naming a hash algorithm.
#' @param cache drake cache
#' @param verbose whether to print console messages
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load the canonical example for drake.
#' # Run the project and return the internal master configuration list.
#' config <- make(my_plan)
#' # Locate the storr cache.
#' cache <- config$cache
#' # Get the short hash algorithm of the cache.
#' short_hash(cache)
#' })
#' }
short_hash <- function(
  cache = drake::get_cache(verbose = verbose),
  verbose = verbose
){
  if (!cache$exists(key = "short_hash_algo", namespace = "config")){
    return(NULL)
  }
  chosen_algo <- cache$get("short_hash_algo", namespace = "config")
  check_storr_short_hash(cache = cache, chosen_algo = chosen_algo)
  cache$get("short_hash_algo", namespace = "config")
}

#' @title Return the default short hash algorithm for \code{make()}.
#' @export
#' @seealso \code{\link{make}}, \code{\link{available_hash_algos}}
#' @description See the advanced storage tutorial
#' at \url{https://wlandau-lilly.github.io/drake/articles/storage.html}
#' for details.
#' @details
#' The short algorithm must be among \code{\link{available_hash_algos}{}},
#' which is just the collection of algorithms available to the `algo`
#' argument in \code{digest::digest()}. \cr \cr
#'
#' If you express no preference for a hash, drake will use
#' the short hash for the existing project, or
#' \code{\link{default_short_hash_algo}()} for a new project.
#' If you do supply a hash algorithm, it will only apply to
#' fresh projects (see \code{\link{clean}(destroy = TRUE)}).
#' For a project that already exists, if you supply a hash algorithm,
#' drake will warn you and then ignore your choice, opting instead for
#' the hash algorithm already chosen for the project
#' in a previous \code{make()}. \cr \cr
#'
#' Drake uses both a short hash algorithm
#' and a long hash algorithm. The shorter hash has fewer characters,
#' and it is used to generate the names of internal cache files
#' and auxiliary files. The decision for short names is important
#' because Windows places restrictions on the length of file paths.
#' On the other hand, some internal hashes in drake are
#' never used as file names, and those hashes can use a longer hash
#' to avoid collisions.
#'
#' @return A character vector naming a hash algorithm.
#'
#' @param cache optional drake cache.
#' When you \code{\link{configure_cache}(cache)} without
#' supplying a short hash algorithm,
#' \code{default_short_hash_algo(cache)} is the short
#' hash algorithm that drake picks for you.
#'
#' @examples
#' default_short_hash_algo()
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load the canonical example for drake.
#' # Run the project and return the internal master configuration list.
#' config <- make(my_plan)
#' # Locate the storr cache.
#' cache <- config$cache
#' # Get the default short hash algorithm of an existing cache.
#' default_short_hash_algo(cache)
#' })
#' }
default_short_hash_algo <- function(cache = NULL) {
  out <- "xxhash64"
  if (is.null(cache)){
    return(out)
  }
  if (cache$exists(key = "short_hash_algo", namespace = "config")){
    out <- cache$get(
      key = "short_hash_algo",
      namespace = "config"
    )
  }
  if ("storr" %in% class(cache)){
    out <- cache$driver$hash_algorithm
  }
  out
}

#' @title Return the default long hash algorithm for \code{make()}.
#' @export
#' @seealso \code{\link{make}}, \code{\link{available_hash_algos}}
#' @description See the advanced storage tutorial
#' at \url{https://wlandau-lilly.github.io/drake/articles/storage.html}
#' for details.
#' @details
#' The long algorithm must be among \code{\link{available_hash_algos}{}},
#' which is just the collection of algorithms available to the `algo`
#' argument in \code{digest::digest()}. \cr \cr
#'
#' If you express no preference for a hash, drake will use
#' the long hash for the existing project, or
#' \code{\link{default_long_hash_algo}()} for a new project.
#' If you do supply a hash algorithm, it will only apply to
#' fresh projects (see \code{\link{clean}(destroy = TRUE)}).
#' For a project that already exists, if you supply a hash algorithm,
#' drake will warn you and then ignore your choice, opting instead for
#' the hash algorithm already chosen for the project
#' in a previous \code{make()}. \cr \cr
#'
#' Drake uses both a short hash algorithm
#' and a long hash algorithm. The shorter hash has fewer characters,
#' and it is used to generate the names of internal cache files
#' and auxiliary files. The decision for short names is important
#' because Windows places restrictions on the length of file paths.
#' On the other hand, some internal hashes in drake are
#' never used as file names, and those hashes can use a longer hash
#' to avoid collisions.
#'
#' @return A character vector naming a hash algorithm.
#'
#' @param cache optional drake cache.
#' When you \code{\link{configure_cache}(cache)} without
#' supplying a long hash algorithm,
#' \code{default_long_hash_algo(cache)} is the long
#' hash algorithm that drake picks for you.
#'
#' @examples
#' default_long_hash_algo()
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load the canonical example for drake.
#' # Run the project and return the internal master configuration list.
#' config <- make(my_plan)
#' # Locate the storr cache.
#' cache <- config$cache
#' # Get the default long hash algorithm of an existing cache.
#' default_long_hash_algo(cache)
#' })
#' }
default_long_hash_algo <- function(cache = NULL) {
  out <- "sha256"
  if (is.null(cache)){
    return(out)
  }
  if (cache$exists(key = "long_hash_algo", namespace = "config")){
    out <- cache$get(
      key = "long_hash_algo",
      namespace = "config"
    )
  }
  out
}

check_storr_short_hash <- function(cache, chosen_algo){
  if (!inherits(cache, "storr")){
    return()
  }
  true_algo <- cache$driver$hash_algorithm
  if (!identical(true_algo, chosen_algo)){
    warning(
      "The storr-based cache actually uses ", true_algo,
      " for the short hash algorithm, but ", chosen_algo,
      " was also supplied. Reverting to ", true_algo, ".",
      call. = FALSE
    )
    cache$set(
      key = "short_hash_algo",
      value = true_algo,
      namespace = "config"
    )
  }
}

#' @title Get the hash keys of all the targets and imports
#' in the cache.
#' @description A hash is a fingerprint of an object's value.
#' Together, the hash keys of all your targets and imports
#' represent the state of your project.
#' Use \code{drake_hash_log()} to generate a data frame
#' with the hash keys of all the targets and imports
#' stored in your cache.
#' This function is particularly useful if you are
#' storing your drake project in a version control repository.
#' The cache has a lot of tiny files, so you should not put it
#' under version control. Instead, save the output
#' of \code{drake_hash_log()} as a text file after each \code{\link{make}()},
#' and put the text file under version control.
#' That way, you have a changelog of your project's results.
#' See the examples below for details.
#' @details Depending on your project's
#' history, the targets may be different than the ones
#' in your workflow plan data frame.
#' Also, the keys depend on the short hash algorithm
#' of your cache (default: \code{\link{default_short_hash_algo}()}).
#' @seealso \code{\link{default_short_hash_algo}},
#' \code{\link{default_long_hash_algo}},
#' \code{\link{short_hash}},
#' \code{\link{long_hash}},
#' \code{\link{cached}},
#' \code{\link{get_cache}}
#' @export
#' @return Data frame of the hash keys of the targets and imports
#' in the cache
#' @param cache drake cache. See \code{\link{new_cache}()}.
#' If supplied, \code{path} and \code{search} are ignored.
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search logical. If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @param verbose whether to print console messages
#' @param jobs number of jobs/workers for parallel processing
#' @param targets_only logical, whether to output information
#' only on the targets in your workflow plan data frame.
#' If \code{targets_only} is \code{FALSE}, the output will
#' include the hashes of both targets and imports. 
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # Load drake's canonical example.
#' load_basic_example()
#' # Run the project, build all the targets.
#' make(my_plan)
#' # Get a data frame of all the hash keys.
#' # If you want a changelog, be sure to do this after every make().
#' hashes <- drake_hash_log()
#' head(hashes)
#' # Save the hash log as a flat text file.
#' write.table(
#'   x = hashes,
#'   file = "hash_log.txt",
#'   quote = FALSE,
#'   row.names = FALSE
#' )
#' # At this point, put hash_log.txt under version control
#' # (e.g. with 'git add hash_log.txt') alongside your code.
#' # Now, every time you run your project, your commit history
#' # of hash_lot.txt is a changelog of the project's results.
#' # It shows which targets and imports changed on every commit.
#' # It is extremely difficult to track your results this way
#' # by putting the raw '.drake/' cache itself under version control.
#' })
#' }
drake_hash_log <- function(
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = TRUE,
  jobs = 1,
  targets_only = FALSE
){
  if (is.null(cache)){
    return(
      data.frame(
        name = character(0),
        type = character(0),
        hash = character(0)
      )
    )
  }
  out <- lightly_parallelize(
    X = cache$list(),
    FUN = single_hash_log,
    jobs = jobs,
    cache = cache
  ) %>%
    do.call(what = rbind)
  if (targets_only){
    out <- out[out$type == "target", ]
  }
  out
}

single_hash_log <- function(key, cache){
  hash <- cache$get_hash(key = key)
  imported <- get_from_subspace(
    key = key,
    subspace = "imported",
    namespace = "meta",
    cache = cache
  )
  imported <- ifelse(is.na(imported), TRUE, imported)
  type <- ifelse(imported, "import", "target")
  data.frame(name = key, type = type, hash = hash, stringsAsFactors = FALSE)
}
