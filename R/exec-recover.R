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
    color = colors["recover"],
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
      algo = config$hash_algorithm
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
    algo = config$hash_algorithm,
    serialize = FALSE
  )
}
