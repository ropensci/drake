recover_target <- function(target, meta, config) {
  hash <- old_recovery_hash(target = target, meta = meta, config = config)
  if (is.na(hash)) {
    return(FALSE)
  }
  value <- config$cache$get(hash, namespace = "recover", use_cache = FALSE)
  config$cache$duplicate(
    key_src = value[1],
    key_dest = value[1],
    namespace = config$cache$default_namespace
  )
  config$cache$duplicate(
    key_src = value[2],
    key_dest = value[2],
    namespace = "meta"
  )
  set_progress(
    target = target,
    meta = list(imported = FALSE),
    value = "done",
    config = config
  )
  TRUE
}

old_recovery_hash <- function(target, meta, config) {
  if (!config$recover) {
    return(NA_character_)
  }
  old_hash <- safe_get_hash(
    key = target,
    namespace = "recover",
    config = config
  )
  new_hash <- recovery_hash(target = target, meta = meta, config = config)
  if (old_hash != new_hash) {
    return(NA_character_)
  }
  old_hash
}

new_recovery_hash <- function(target, meta, config) {
  if (is.null(meta$trigger$value)) {
    change_hash <- NA_character_
  } else {
    change_hash <- digest::digest(
      meta$trigger$value,
      algo = config$cache$driver$hash_algorithm
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
    algo = config$cache$driver$hash_algorithm,
    serialize = FALSE
  )
}
