cache_vers_check <- function(cache) {
  if (is.null(cache)) {
    return(character(0))
  }
  old <- get_cache_version(cache = cache)
  if (compareVersion(old, "6.2.1") <= 0) {
    paste0(
      "This project was last run with drake version ",
      old, ".\nYour cache is not compatible with the current ",
      "version of drake (",
      packageVersion("drake"), ").\nTo run your project with version ",
      packageVersion("drake"), ", either:\n",
      "    1. Run `clean(destroy = TRUE)` ",
      "to clear the cache first (recommended) or\n",
      "    2. Run `make(force = TRUE)` ",
      "to overwrite targets in the cache on the fly (which saves more ",
      "of your work but may cause issues like ",
      " https://github.com/ropensci/drake/issues/725).\n",
      "Be warned: if you do either, ",
      "all you targets will run from scratch.\nYou may wish to ",
      "downgrade drake to version ", old, " instead."
    )
  } else {
    character(0)
  }
}

cache_vers_stop <- function(cache){
  msg <- cache_vers_check(cache)
  if (length(msg)) {
    stop(msg, call. = FALSE)
  }
}

cache_vers_warn <- function(cache){
  msg <- cache_vers_check(cache)
  if (length(msg)) {
    warning(msg, call. = FALSE)
  }
}

enforce_compatible_config <- function(config) {
  # Placeholder function in case we need it.
  config
}

get_cache_version <- function(cache) {
  if (cache$exists(key = "drake_version", namespace = "session")) {
    cache$get(key = "drake_version", namespace = "session")
  } else if (cache$exists(key = "sessionInfo", namespace = "session")) {
    session_info <- drake_get_session_info(cache = cache)
    all_pkgs <- c(
      session_info$otherPkgs, # nolint
      session_info$loadedOnly # nolint
    )
    as.character(all_pkgs$drake$Version)
  } else {
    as.character(utils::packageVersion("drake"))
  }
}
