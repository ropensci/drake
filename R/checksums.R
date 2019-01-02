mc_get_checksum <- function(target, config) {
  paste(
    safe_get_hash(
      key = target,
      namespace = config$cache$default_namespace,
      config = config
    ),
    safe_get_hash(key = target, namespace = "kernels", config = config),
    safe_get_hash(key = target, namespace = "meta", config = config),
    safe_get_hash(key = target, namespace = "attempt", config = config),
    mc_get_outfile_checksum(target, config),
    sep = " "
  )
}

mc_get_outfile_checksum <- function(target, config) {
  deps <- config$layout[[target]]$deps_build
  files <- sort(unique(as.character(deps$file_out)))
  out <- vapply(
    X = files,
    FUN = rehash_file,
    FUN.VALUE = character(1),
    config = config
  )
  out <- paste(out, collapse = "")
  digest::digest(
    out,
    algo = config$cache$driver$hash_algorithm,
    serialize = FALSE
  )
}

mc_is_good_checksum <- function(target, checksum, config) {
  # Not actually reached, but may come in handy later.
  # nocov start
  if (!length(checksum)) {
    mc_warn_no_checksum(target = target, config = config)
    return(TRUE)
  }
  if (identical("failed", get_progress_single(target, cache = config$cache))) {
    return(TRUE) # covered with parallel processes # nocov
  }
  # nocov end
  local_checksum <- mc_get_checksum(target = target, config = config)
  if (!identical(local_checksum, checksum)) {
    return(FALSE)
  }
  all(
    vapply(
      X = unlist(strsplit(local_checksum, " "))[1:3], # Exclude attempt flag (often NA). # nolint
      config$cache$exists_object,
      FUN.VALUE = logical(1)
    )
  )
}

mc_is_good_outfile_checksum <- function(target, checksum, config) {
  if (!length(checksum)) {
    mc_warn_no_checksum(target = target, config = config)
    return(TRUE)
  }
  if (identical("failed", get_progress_single(target, cache = config$cache))) {
    return(TRUE) # covered with parallel processes # nocov
  }
  identical(checksum, mc_get_outfile_checksum(target = target, config = config))
}

mc_wait_checksum <- function(
  target,
  checksum,
  config,
  timeout = 300,
  criterion = mc_is_good_checksum
) {
  i <- 0
  time_left <- timeout
  while (time_left > 0) {
    if (criterion(target, checksum, config)) {
      return()
    } else {
      sleep <- config$sleep(max(0L, i))
      Sys.sleep(sleep)
      time_left <- time_left - sleep
    }
    i <- i + 1
  }
  drake_error(
    "Target `", target, "` did not download from your ",
    "network file system. Checksum verification timed out after about ",
    timeout, " seconds.", config = config
  )
}

mc_wait_outfile_checksum <- function(target, checksum, config, timeout = 300) {
  mc_wait_checksum(
    target = target,
    checksum = checksum,
    config = config,
    timeout = timeout,
    criterion = mc_is_good_outfile_checksum
  )
}

mc_warn_no_checksum <- function(target, config) {
  drake_warning(
    "No checksum available for target ", target, ".",
    config = config
  )
}
