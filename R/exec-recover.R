recover_target <- function(target, meta, config) {
  if (is.null(config$recover)) {
    return(FALSE)
  }
  value <- recovery_metadata(target, meta, config)
  if (length(value) < 2L && is.na(value)) {
    return(FALSE)
  }
  log_msg(
    "recover",
    target,
    target = target,
    config = config,
    color = colors["recover"],
    tier = 1L
  )
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

recovery_metadata <- function(target, meta, config) {
  hash <- recovery_hash(target = target, meta = meta, config = config)
  if (!config$cache$exists(hash, namespace = "recover")) {
    return(NA_character_)
  }
  value <- config$cache$get(hash, namespace = "recover", use_cache = FALSE)
  for (i in seq_along(value)) {
    if (!config$cache$exists_object(value[i])) {
      return(NA_character_)
    }
  }
  value
}

recovery_hash <- function(target, meta, config) {
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
