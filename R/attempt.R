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

set_attempt_flag <- function(config){
  suppressWarnings(
    config$cache$set(
      key = "attempt", value = TRUE, namespace = "session")
  )
  invisible()
}
