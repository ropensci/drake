meta_list <- function(targets, config) {
  console_many_targets(targets = targets,
    pattern = "check", color = "check",
    config = config)
  out <- lightly_parallelize(
    X = targets,
    FUN = meta,
    jobs = config$jobs,
    config = config
  )
  names(out) <- lapply(out, "[[", "target") %>%
    unlist
  out
}

meta <- function(target, config) {
  meta <- list(
    target = target,
    imported = !(target %in% config$plan$target),
    missing = !target_exists(target = target, config = config)
  )
  trigger <- get_trigger(target = target, config = config)
  # Need to make sure meta includes all these
  # fields at the beginning of build_in_hook(),
  # but only after drake decides to actually build the target.
  if (trigger %in% triggers_with_command()){
    meta$command <- get_command(target = target, config = config)
  }
  if (trigger %in% triggers_with_depends()){
    meta$depends <- dependency_hash(target = target, config = config)
  }
  if (trigger %in% triggers_with_file()){
    meta$file <- file_hash(target = target, config = config)
  }
  meta
}

finish_meta <- function(target, meta, config){
  if (is_file(target)) {
    # Keep an updated modification time for each file.
    meta$mtime <- file.mtime(drake::drake_unquote(target))
    # If the target is a file output, then we know
    # it needs to be rehashed.
    if (!meta$imported){
      meta$file <- rehash_file(target = target, config = config)
    }
  }
  # If the user selected a non-default trigger,
  # some of these fields might not be populated.
  # Empty fields need to be filled so that the target
  # can appear up to date in the next make().
  if (is.null(meta$file)){
    meta$file <- file_hash(target = target, config = config)
  }
  if (is.null(meta$command)){
    meta$command <- get_command(target = target, config = config)
  }
  if (is.null(meta$depends)){
    meta$depends <- dependency_hash(target = target, config = config)
  }
  meta
}

dependency_hash <- function(target, config) {
  dependencies(target, config) %>%
    self_hash(config = config) %>%
    digest::digest(algo = config$long_hash_algo)
}

self_hash <- Vectorize(function(target, config) {
  if (kernel_exists(target = target, config = config)) {
    config$cache$get_hash(target, namespace = "kernels")
  } else {
    as.character(NA)
  }
},
"target", USE.NAMES = FALSE)

rehash_file <- function(target, config) {
  digest::digest(
    object = drake::drake_unquote(target),
    algo = config$long_hash_algo,
    file = TRUE,
    serialize = FALSE
  )
}

should_rehash_file <- function(filename, new_mtime, old_mtime,
  size_cutoff){
  do_rehash <- file.size(filename) < size_cutoff | new_mtime > old_mtime
  if (is.na(do_rehash)){
    do_rehash <- TRUE
  }
  do_rehash
}

file_hash <- function(target, config, size_cutoff = 1e5) {
  if (is_file(target)) {
    filename <- drake::drake_unquote(target)
  } else {
    return(as.character(NA))
  }
  if (!file.exists(filename))
    return(as.character(NA))
  old_mtime <- ifelse(
    exists_in_subspace(
      key = target,
      subspace = "mtime",
      namespace = "meta",
      cache = config$cache
    ),
    get_from_subspace(
      key = target,
      subspace = "mtime",
      namespace = "meta",
      cache = config$cache
    ),
    -Inf
  )
  new_mtime <- file.mtime(filename)
  do_rehash <- should_rehash_file(
    filename = filename,
    new_mtime = new_mtime,
    old_mtime = old_mtime,
    size_cutoff = size_cutoff)
  if (do_rehash){
    rehash_file(target = target, config = config)
  } else {
    config$cache$get(key = target, namespace = "kernels")
  }
}
