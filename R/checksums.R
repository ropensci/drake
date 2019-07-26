wait_outfile_checksum <- function(target, checksum, config, timeout = 300) {
  wait_checksum(
    target = target,
    checksum = checksum,
    config = config,
    timeout = timeout,
    criterion = is_good_outfile_checksum
  )
}

wait_checksum <- function(
  target,
  checksum,
  config,
  timeout = 300,
  criterion = is_good_checksum
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
  msg <- paste0(
    "Target `", target, "` did not download from your ",
    "network file system. Checksum verification timed out after about ",
    timeout, " seconds."
  )
  drake_log(paste("Error:", msg), config = config)
  stop(msg, call. = FALSE)
}

is_good_checksum <- function(target, checksum, config) {
  # Not actually reached, but may come in handy later.
  # nocov start
  if (!length(checksum)) {
    warn_no_checksum(target = target, config = config)
    return(TRUE)
  }
  if (identical("failed", get_progress_single(target, cache = config$cache))) {
    return(TRUE) # covered with parallel processes # nocov
  }
  # nocov end
  local_checksum <- get_checksum(target = target, config = config)
  if (!identical(local_checksum, checksum)) {
    return(FALSE)
  }
  all(
    vapply(
      X = unlist(strsplit(local_checksum, " "))[1:2],
      config$cache$exists_object,
      FUN.VALUE = logical(1)
    )
  )
}

is_good_outfile_checksum <- function(target, checksum, config) {
  if (!length(checksum)) {
    warn_no_checksum(target = target, config = config)
    return(TRUE)
  }
  if (identical("failed", get_progress_single(target, cache = config$cache))) {
    return(TRUE) # covered with parallel processes # nocov
  }
  identical(checksum, get_outfile_checksum(target = target, config = config))
}

get_checksum <- function(target, config) {
  paste(
    safe_get_hash(
      key = target,
      namespace = config$cache$default_namespace,
      config = config
    ),
    safe_get_hash(key = target, namespace = "meta", config = config),
    get_outfile_checksum(target, config),
    sep = " "
  )
}

get_outfile_checksum <- function(target, config) {
  deps <- config$layout[[target]]$deps_build
  files <- sort(unique(as.character(deps$file_out)))
  out <- vapply(
    X = files,
    FUN = rehash_storage,
    FUN.VALUE = character(1),
    config = config
  )
  out <- paste(out, collapse = "")
  digest::digest(
    out,
    algo = config$hash_algorithm,
    serialize = FALSE
  )
}

warn_no_checksum <- function(target, config) {
  msg <- paste0("No checksum available for target ", target, ".")
  drake_log(paste("Warning:", msg), config = config)
  warning(msg, call. = FALSE)
}
