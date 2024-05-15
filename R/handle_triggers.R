handle_triggers <- function(target, meta, config) {
  class(target) <- handle_triggers_class(target, config)
  handle_triggers_impl(target, meta, config)
}

handle_triggers_class <- function(target, config) {
  if (is_subtarget(target, config)) {
    return("subtarget")
  }
  if (is_registered_dynamic(target, config)) {
    return("dynamic_registered")
  }
  if (is_dynamic(target, config)) {
    return("dynamic_unregistered")
  }
  "static"
}

handle_triggers_impl <- function(target, meta, config) {
  UseMethod("handle_triggers_impl")
}

#' @export
handle_triggers_impl.subtarget <- function(target, meta, config) { # nolint
  FALSE
}

#' @export
handle_triggers_impl.dynamic_registered <- function(target, meta, config) { # nolint
  FALSE
}

#' @export
handle_triggers_impl.dynamic_unregistered <- function(target, meta, config) { # nolint
  target <- unclass(target)
  static_ok <- !any_static_triggers(target, meta, config) ||
    recover_target(target, meta, config)
  dynamic_ok <- !check_trigger_dynamic(target, meta, config)
  register_subtargets(target, static_ok, dynamic_ok, config)
  TRUE
}

#' @export
handle_triggers_impl.static <- function(target, meta, config) { # nolint
  target <- unclass(target)
  any_triggers <- any_static_triggers(target, meta, config) ||
    any_subtargetlike_triggers(target, meta, config)
  !any_triggers || recover_target(target, meta, config)
}

recover_target <- function(target, meta, config) {
  if (!config$settings$recover) {
    return(FALSE)
  }
  key <- recovery_key_impl(target = target, meta = meta, config = config)
  if (!config$cache$exists(key, namespace = "recover")) {
    return(FALSE)
  }
  meta_hash <- config$cache$get_hash(
    key,
    namespace = "recover"
  )
  recovery_meta <- config$cache$driver$get_object(meta_hash)
  value_hash <- recovery_meta$hash
  exists_data <- config$cache$exists_object(meta_hash) &&
    config$cache$exists_object(value_hash)
  if (!exists_data) {
    return(FALSE) # nocov # Should not happen, just to be safe...
  }
  meta_old <- drake_meta_old(target, config)
  on.exit(config$meta_old[[target]] <- meta_old)
  config$meta_old[[target]] <- recovery_meta
  meta <- subsume_old_meta(target, meta, config)
  if (any_subtargetlike_triggers(target, meta, config)) {
    return(FALSE)
  }
  if (!is_dynamic(target, config)) {
    config$logger$target(target, "recover")
  }
  config$logger$disk(
    "recovered target originally stored on",
    recovery_meta$date,
    target = target
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
    value = "done",
    config = config
  )
  TRUE
}

recover_subtarget <- function(subtarget, parent, config) {
  meta <- drake_meta_(subtarget, config)
  class(subtarget) <- "subtarget"
  recover_target(subtarget, meta, config)
}

recovery_key <- function(target, meta, config) {
  if (is_subtarget(target, config)) {
    class(target) <- "subtarget"
  }
  recovery_key_impl(target, meta, config)
}

recovery_key_impl <- function(target, meta, config) {
  UseMethod("recovery_key_impl")
}

#' @export
recovery_key_impl.subtarget <- function(target, meta, config) {
  parent <- subtarget_parent(target, config)
  parent_meta <- drake_meta_(parent, config)
  parent_key <- recovery_key(parent, parent_meta, config)
  x <- c(unclass(target), parent_key)
  x <- paste(x, collapse = "|")
  config$cache$digest(x, serialize = FALSE)
}

#' @export
recovery_key_impl.default <- function(target, meta, config, ...) {
  change_hash <- ifelse(
    is.null(meta$trigger$value),
    NA_character_,
    config$cache$digest(meta$trigger$value)
  )
  x <- c(
    meta$command,
    meta$dependency_hash,
    meta$input_file_hash,
    meta$output_file_hash,
    meta$trigger$mode,
    meta$format,
    as.character(meta$seed),
    safe_deparse(meta$trigger$condition, backtick = TRUE),
    change_hash
  )
  x <- paste(x, collapse = "|")
  config$cache$digest(x, serialize = FALSE)
}

any_static_triggers <- function(target, meta, config) {
  if (check_triggers_stage1(target, meta, config)) {
    return(TRUE)
  }
  condition <- check_trigger_condition(target, meta, config)
  if (is_final_trigger(condition)) {
    return(condition)
  }
  if (condition) {
    return(TRUE)
  }
  if (check_triggers_stage2(target, meta, config)) {
    return(TRUE)
  }
  FALSE
}

any_subtargetlike_triggers <- function(target, meta, config) {
  if (check_trigger_format_file(target, meta, config)) {
    return(TRUE)
  }
  FALSE
}

any_subtarget_triggers <- function(target, subtargets, config) {
  any(check_subtarget_triggers(target, subtargets, config))
}

check_triggers_stage1 <- function(target, meta, config) {
  if (check_trigger_imported(target, meta, config)) {
    return(TRUE)
  }
  if (check_trigger_missing(target, meta, config)) {
    return(TRUE)
  }
  FALSE
}

check_triggers_stage2 <- function(target, meta, config) {
  if (check_trigger_command(target, meta, config)) {
    return(TRUE)
  }
  if (check_trigger_depend(target, meta, config)) {
    return(TRUE)
  }
  if (check_trigger_file(target, meta, config)) {
    return(TRUE)
  }
  if (check_trigger_seed(target, meta, config)) {
    return(TRUE)
  }
  if (check_trigger_format(target, meta, config)) {
    return(TRUE)
  }
  if (check_trigger_change(target, meta, config)) {
    return(TRUE)
  }
  FALSE
}

check_trigger_imported <- function(target, meta, config) {
  if (meta$imported) {
    return(TRUE)
  }
  FALSE
}

check_trigger_missing <- function(target, meta, config) {
  if (meta$missing) {
    config$logger$disk("trigger missing", target = target)
    return(TRUE)
  }
  FALSE
}

check_trigger_condition <- function(target, meta, config) {
  condition <- trigger_condition(target = target, meta = meta, config = config)
  stopifnot(is.logical(condition))
  if (condition) {
    config$logger$disk("trigger condition", target = target)
  }
  return(condition)
}

check_trigger_command <- function(target, meta, config) {
  if (identical(meta$trigger$command, TRUE)) {
    if (trigger_command(target, meta, config)) {
      config$logger$disk("trigger command", target = target)
      return(TRUE)
    }
  }
  FALSE
}

check_trigger_depend <- function(target, meta, config) {
  if (identical(meta$trigger$depend, TRUE)) {
    if (trigger_depend(target, meta, config)) {
      config$logger$disk("trigger depend", target = target)
      return(TRUE)
    }
  }
  FALSE
}

check_trigger_file <- function(target, meta, config) {
  if (identical(meta$trigger$file, TRUE)) {
    if (trigger_file(target, meta, config)) {
      config$logger$disk("trigger file", target = target)
      return(TRUE)
    }
  }
  FALSE
}

check_trigger_seed <- function(target, meta, config) {
  if (identical(meta$trigger$seed, TRUE)) {
    if (trigger_seed(target, meta, config)) {
      config$logger$disk("trigger seed", target = target)
      return(TRUE)
    }
  }
  FALSE
}

check_trigger_change <- function(target, meta, config) {
  if (!is.null(meta$trigger$change)) {
    if (trigger_change(target = target, meta = meta, config = config)) {
      config$logger$disk("trigger change", target = target)
      return(TRUE)
    }
  }
  config$logger$disk("skip", target = target)
  FALSE
}

check_trigger_format <- function(target, meta, config) {
  if (identical(meta$trigger$format, TRUE)) {
    if (trigger_format(target, meta, config)) {
      config$logger$disk("trigger format", target = target)
      return(TRUE)
    }
  }
  FALSE
}

check_trigger_dynamic <- function(target, meta, config) {
  if (identical(meta$trigger$depend, TRUE)) {
    if (trigger_dynamic(target, meta, config)) {
      config$logger$disk("trigger depend (dynamic)", target = target)
      return(TRUE)
    }
  }
  FALSE
}

check_trigger_format_file <- function(target, meta, config) {
  if (identical(meta$trigger$file, TRUE) && meta$format == "file") {
    if (trigger_format_file(target, meta, config)) {
      config$logger$disk("trigger file (format file)", target = target)
      return(TRUE)
    }
  }
  FALSE
}

check_subtarget_triggers <- function(target, subtargets, config) {
  out <- target_missing(subtargets, config)
  spec <- config$spec[[target]]
  format <- spec$format %||NA% "none"
  if (identical(spec$trigger$file, TRUE) && format == "file" && !all(out)) {
    i <- !out
    out[i] <- out[i] | check_trigger_subtarget_format_file(
      subtargets[i],
      target,
      config
    )
  }
  out[is.na(out)] <- TRUE
  if (any(out)) {
    config$logger$disk("trigger subtarget (special)", target = target)
  }
  out
}

check_trigger_subtarget_format_file <- function( # nolint
  subtargets,
  parent,
  config
) {
  ht_set(config$ht_is_subtarget, subtargets)
  out <- lightly_parallelize(
    X = subtargets,
    FUN = check_trigger_subtarget_format_file_impl,
    jobs = config$settings$jobs_preprocess,
    parent = parent,
    config = config
  )
  unlist(out)
}

check_trigger_subtarget_format_file_impl <- function( # nolint
  subtarget,
  parent,
  config
) {
  meta <- drake_meta_(subtarget, config)
  trigger_format_file(subtarget, meta, config)
}

trigger_command <- function(target, meta, config) {
  if (is.null(meta$command)) {
    return(FALSE)
  }
  meta_old <- drake_meta_old(target, config)
  command <- meta_elt(field = "command", meta = meta_old)
  !identical(command, meta$command)
}

trigger_depend <- function(target, meta, config) {
  if (is.null(meta$dependency_hash)) {
    return(FALSE)
  }
  meta_old <- drake_meta_old(target, config)
  dependency_hash <- meta_elt(field = "dependency_hash", meta = meta_old)
  !identical(dependency_hash, meta$dependency_hash)
}

trigger_file <- function(target, meta, config) {
  if (!length(target) || !length(config) || !length(meta)) {
    return(FALSE)
  }
  if (trigger_file_missing(target, meta, config)) {
    return(TRUE)
  }
  if (trigger_file_hash(target, meta, config)) {
    return(TRUE)
  }
  FALSE
}

trigger_file_missing <- function(target, meta, config) {
  file_out <- config$spec[[target]]$deps_build$file_out
  for (file in file_out) {
    if (!file.exists(config$cache$decode_path(file))) {
      return(TRUE)
    }
  }
  FALSE
}

trigger_file_hash <- function(target, meta, config) {
  for (hash_name in c("input_file_hash", "output_file_hash")) {
    meta_old <- drake_meta_old(target, config)
    old_file_hash <- meta_elt(field = hash_name, meta = meta_old)
    if (!identical(old_file_hash, meta[[hash_name]])) {
      return(TRUE)
    }
  }
  FALSE
}

trigger_seed <- function(target, meta, config) {
  meta_old <- drake_meta_old(target, config)
  seed <- meta_elt(field = "seed", meta = meta_old)
  !identical(as.integer(seed), as.integer(meta$seed))
}

trigger_format <- function(target, meta, config) {
  meta_old <- drake_meta_old(target, config)
  format_new <- meta$format
  format_old <- meta_old$format
  if (is.null(format_new) || is.null(format_old)) {
    return(FALSE)
  }
  !identical(format_new, format_old)
}

trigger_condition <- function(target, meta, config) {
  if (!length(target) || !length(config) || !length(meta)) {
    return(FALSE)
  }
  if (is.language(meta$trigger$condition)) {
    try_load_deps(
      config$spec[[target]]$deps_condition$memory,
      config = config
    )
    value <- eval(meta$trigger$condition, envir = config$envir_targets)
  } else {
    value <- meta$trigger$condition
  }
  value <- as.logical(value)
  if (length(value) != 1 || !is.logical(value)) {
    msg <- paste0(
      "The `condition` trigger must evaluate to a logical of length 1. ",
      "got `", value, "` for target ", target, "."
    )
    config$logger$disk(paste("Error:", msg))
    stop0(msg)
  }
  condition_decision(value = value, mode = meta$trigger$mode)
}

condition_decision <- function(value, mode) {
  if (identical(mode, "whitelist") && identical(value, TRUE)) {
    return(TRUE)
  }
  if (identical(mode, "blacklist") && identical(value, FALSE)) {
    return(as_final_trigger(FALSE))
  }
  if (identical(mode, "condition")) {
    return(as_final_trigger(value))
  }
  FALSE
}

as_final_trigger <- function(x) {
  structure(x, class = "final_trigger")
}

is_final_trigger <- function(x) {
  inherits(x, "final_trigger")
}

trigger_change <- function(target, meta, config) {
  if (!length(target) || !length(config) || !length(meta)) {
    return(FALSE)
  }
  if (!config$cache$exists(key = target, namespace = "change")) {
    return(TRUE) # nocov
  }
  old_value <- config$cache$get(
    key = target,
    namespace = "change",
    use_cache = FALSE
  )
  !identical(old_value, meta$trigger$value)
}

trigger_dynamic <- function(target, meta, config) {
  meta_old <- drake_meta_old(target, config)
  old_hash <- meta_elt(field = "dynamic_dependency_hash", meta = meta_old)
  if (!identical(meta$dynamic_dependency_hash, old_hash)) {
    return(TRUE)
  }
  if (!identical(meta$max_expand, meta_old$max_expand)) {
    return(TRUE)
  }
  FALSE
}

trigger_format_file <- function(target, meta, config) {
  meta_old <- drake_meta_old(target, config)
  hash_new <- meta$format_file_hash
  hash_old <- meta_old$format_file_hash
  !identical(hash_new, hash_old)
}
