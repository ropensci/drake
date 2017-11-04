#' @title Function migrate
#' @export
#' @seealso \code{\link{rescue_cache}}, \code{\link{make}}
#' @param path Full path to the cache
#' @description Migrate a project/cache from drake 4.4.0 or earlier
#' to be compatible with the version of drake on your system.
#' @details Versions after drake have a different internal structure for the cache.
#' This means projects built with drake 4.4.0 or before are not compatible
#' with projects built with a later version of drake. migrate() converts
#' an old cache to a format compatible with the version of drake
#' installed on your system.
migrate <- function(path = drake::default_cache_path()){
  do_migrate <- should_migrate(path = path, throw_error = FALSE)
  if (!do_migrate){
    cat("No need to migrate the cache. Returning.\n")
  }
  
}

assert_compatible_cache <- function(cache){
  tmp <- should_migrate(cache = cache, throw_error = TRUE)
  invisible()
}

backup_cache_path <- function(path, old){
  newpath <- paste0(path, "_backup_drake_", old)
}

should_migrate <- function(path = NULL, cache = drake::this_cache(path = path),
  throw_error = TRUE){
  if (is.null(cache)){
    return()
  }
  err <- try(
    old <- session(cache = cache)$otherPkgs$drake$Version, silent = TRUE) # nolint
  if (inherits(err, "try-error")){
    return(FALSE)
  }
  comparison <- compareVersion(old, "4.4.0")
  if (comparison > 0){
    return(FALSE)
  }
  if (throw_error){
    current <- packageVersion("drake")
    path <- cache$driver$path
    newpath <- backup_cache_path(path = path, old = old)
    stop(
      "The project at '", path, "' was previously built by drake ", old, ". ",
      "You are running drake ", current, ", which is not back-compatible. ",
      "To format your cache for the newer drake, try migrate('", path, "'). ",
      "migrate() restructures the cache in a way that ",
      "preserves the statuses of your targets (up to date vs outdated). ",
      "But in case of errors, migrate() first backs up '", path, "' to '",
      newpath, "'. Alternatively, you can just run your project from scratch ",
      "as is with make(..., force = TRUE)."
    )
  }
  TRUE
}
