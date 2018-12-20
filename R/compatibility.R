cache_vers_check <- function(cache) {
  if (is.null(cache)) {
    return(character(0))
  }
  old <- last_drake_version(cache = cache)
  if (is.null(old)) {
    return(invisible())
  }
  if (compareVersion(old, "5.4.0") < 0) {
    paste0(
      "This project was last run with drake version ",
      old, ".\nYour cache is not compatible with the current ",
      "version of drake (",
      packageVersion("drake"), ").\nTo run your project with version ",
      packageVersion("drake"), ", use make(force = TRUE).\n",
      "But be warned: if you do that, ",
      "all you targets will run from scratch.\nYou may instead wish to ",
      "downgrade drake to version ", old, "."
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
  # TODO: can probably remove this conditional for drake 7.0.0
  if (config$cache$exists("long_hash_algo", namespace = "config")) {
    config$long_hash_algo <- config$cache$get(
      "long_hash_algo",
      namespace = "config"
    )
  } else {
    config$long_hash_algo <- config$cache$driver$hash_algorithm
  }
  config$using_encoded_paths <- using_encoded_paths(cache = config$cache)
  config
}

last_drake_version <- function(cache) {
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
    NULL
  }
}

using_encoded_paths <- function(cache) {
  version <- last_drake_version(cache) %||% "0.0.0"
  compareVersion(version, "6.2.1") > 0L
}
