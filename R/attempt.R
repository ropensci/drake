# Manage the flag that detects if drake has
# attempted to build any targets
# in the current make() session.

get_attempt_flag <- function(config){
  flag <- safe_get(
    key = "attempt",
    namespace = "session",
    config = config
  )
  ifelse(is.na(flag), FALSE, flag)
}

increment_attempt_flag <- function(targets, config){
  flag <- as.logical(length(targets)) || get_attempt_flag(config = config)
  set_attempt_flag(flag = flag, config = config)
}

set_attempt_flag <- function(flag, config){
  config$cache$set(
    key = "attempt", value = flag, namespace = "session")
  invisible()
}
