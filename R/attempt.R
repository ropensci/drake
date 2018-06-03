# Manage the flag that detects if drake has
# attempted to build any targets
# in the current make() session.

get_attempt_flag <- function(config){
  length(config$cache$list(namespace = "attempt")) > 0
}

# Allows different keys for thread safety.
set_attempt_flag <- function(key = "_attempt", config){
  config$cache$set(
    key = as.character(key), value = TRUE, namespace = "attempt")
  invisible()
}
