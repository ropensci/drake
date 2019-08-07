handle_trigger <- function(target, meta, config) {
  !sense_trigger(target, meta, config) ||
    recover_target(target, meta, config)
}

recover_target <- function(target, meta, config) {
  if (!config$recover) {
    return(FALSE)
  }
  key <- recovery_key(target = target, meta = meta, config = config)
  if (!config$cache$exists(key, namespace = "recover")) {
    return(FALSE)
  }
  meta_hash <- config$cache$get_hash(key, namespace = "recover")
  recovery_meta <- config$cache$driver$get_object(meta_hash)
  value_hash <- recovery_meta$hash
  exists_data <- config$cache$exists_object(meta_hash) &&
    config$cache$exists_object(value_hash)
  if (!exists_data) {
    return(FALSE) # nocov # Should not happen, just to be safe...
  }
  log_msg(
    "recover",
    target,
    target = target,
    config = config,
    color = "recover",
    tier = 1L
  )
  log_msg(
    "recovered target originally stored on",
    recovery_meta$date,
    target = target,
    config = config
  )
  config$cache$driver$set_hash(
    key = target,
    namespace = "meta",
    hash = meta_hash
  )
  config$cache$driver$set_hash(
    key = target,
    namespace = config$cache$default_namespace,
    hash = value_hash
  )
  set_progress(
    target = target,
    meta = list(imported = FALSE),
    value = "done",
    config = config
  )
  TRUE
}

recovery_key <- function(target, meta, config) {
  if (is.null(meta$trigger$value)) {
    change_hash <- NA_character_
  } else {
    change_hash <- digest::digest(
      meta$trigger$value,
      algo = config$cache$hash_algorithm
    )
  }
  x <- c(
    meta$command,
    meta$dependency_hash,
    meta$input_file_hash,
    meta$output_file_hash,
    as.character(meta$seed),
    safe_deparse(meta$trigger$condition),
    meta$trigger$mode,
    change_hash
  )
  x <- paste(x, collapse = "|")
  digest::digest(
    x,
    algo = config$cache$hash_algorithm,
    serialize = FALSE
  )
}

sense_trigger <- function(target, meta, config) {
  if (meta$imported) {
    return(TRUE)
  }
  if (meta$missing) {
    log_msg("trigger missing", target = target, config = config)
    return(TRUE)
  }
  condition <- condition_trigger(target = target, meta = meta, config = config)
  if (is.logical(condition)) {
    if (condition) {
      log_msg("trigger condition", target = target, config = config)
    }
    return(condition)
  }
  if (identical(meta$trigger$command, TRUE)) {
    if (command_trigger(target = target, meta = meta, config = config)) {
      log_msg("trigger command", target = target, config = config)
      return(TRUE)
    }
  }
  if (identical(meta$trigger$depend, TRUE)) {
    if (depend_trigger(target = target, meta = meta, config = config)) {
      log_msg("trigger depend", target = target, config = config)
      return(TRUE)
    }
  }
  if (identical(meta$trigger$file, TRUE)) {
    if (file_trigger(target = target, meta = meta, config = config)) {
      log_msg("trigger file", target = target, config = config)
      return(TRUE)
    }
  }
  if (identical(meta$trigger$seed, TRUE)) {
    if (seed_trigger(target = target, meta = meta, config = config)) {
      log_msg("trigger seed", target = target, config = config)
      return(TRUE)
    }
  }
  if (!is.null(meta$trigger$change)) {
    if (change_trigger(target = target, meta = meta, config = config)) {
      log_msg("trigger change", target = target, config = config)
      return(TRUE)
    }
  }
  log_msg("skip", target = target, config = config)
  FALSE
}

command_trigger <- function(target, meta, config) {
  if (is.null(meta$command)) {
    return(FALSE)
  }
  command <- read_from_meta(
    key = target,
    field = "command",
    cache = config$cache
  )
  !identical(command, meta$command)
}

depend_trigger <- function(target, meta, config) {
  if (is.null(meta$dependency_hash)) {
    return(FALSE)
  }
  dependency_hash <- read_from_meta(
    key = target,
    field = "dependency_hash",
    cache = config$cache
  )
  !identical(dependency_hash, meta$dependency_hash)
}

file_trigger <- function(target, meta, config) {
  if (!length(target) || !length(config) || !length(meta)) {
    return(FALSE)
  }
  file_out <- config$layout[[target]]$deps_build$file_out
  for (file in file_out) {
    if (!file.exists(decode_path(file, config))) {
      return(TRUE)
    }
  }
  for (hash_name in c("input_file_hash", "output_file_hash")) {
    old_file_hash <- read_from_meta(
      key = target,
      field = hash_name,
      cache = config$cache
    )
    if (!identical(old_file_hash, meta[[hash_name]])) {
      return(TRUE)
    }
  }
  FALSE
}

seed_trigger <- function(target, meta, config) {
  seed <- read_from_meta(
    key = target,
    field = "seed",
    cache = config$cache
  )
  !identical(seed, meta$seed)
}

condition_trigger <- function(target, meta, config) {
  if (!length(target) || !length(config) || !length(meta)) {
    return(FALSE)
  }
  if (is.language(meta$trigger$condition)) {
    try_load(config$layout[[target]]$deps_condition$memory, config = config)
    value <- eval(meta$trigger$condition, envir = config$eval)
    value <- as.logical(value)
  } else {
    value <- as.logical(meta$trigger$condition)
  }
  if (length(value) != 1 || !is.logical(value)) {
    msg <- paste0(
      "The `condition` trigger must evaluate to a logical of length 1. ",
      "got `", value, "` for target ", target, "."
    )
    drake_log(paste("Error:", msg), config = config)
    stop(msg, call. = FALSE)
  }
  condition_decision(value = value, mode = meta$trigger$mode)
}

condition_decision <- function(value, mode) {
  if (identical(mode, "whitelist") && identical(value, TRUE)) {
    return(TRUE)
  }
  if (identical(mode, "blacklist") && identical(value, FALSE)) {
    return(FALSE)
  }
  if (identical(mode, "condition")) {
    return(value)
  }
  "defer"
}

change_trigger <- function(target, meta, config) {
  if (!length(target) || !length(config) || !length(meta)) {
    return(FALSE)
  }
  if (!config$cache$exists(key = target, namespace = "change")) {
    return(TRUE) # nocov
  }
  old_value <- config$cache$get(key = target, namespace = "change")
  !identical(old_value, meta$trigger$value)
}
