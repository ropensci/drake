meta_list <- function(targets, config) {
  console_many_targets(targets = targets,
    pattern = "check", color = "check",
    config = config)
  out <- lightly_parallelize(
    X = targets,
    FUN = drake_meta,
    jobs = config$jobs,
    config = config
  )
  names(out) <- lapply(out, "[[", "target") %>%
    unlist
  out
}

#' @title Compute the initial pre-build metadata of a target or import.
#' @description The metadata helps determine if the
#' target is up to date or outdated. The metadata of imports
#' is used to compute the metadata of targets.
#' @details Target metadata is computed
#' with `drake_meta()`, and then
#' `drake:::finish_meta()` completes the metadata
#' after the target is built.
#' In other words, the output of `drake_meta()` corresponds
#' to the state of the target immediately before [make()]
#' builds it.
#' See [diagnose()] to read the final metadata of a target,
#' including any errors, warnings, and messages in the last build.
#' @seealso [diagnose()], [dependency_profile()], [make()]
#' @export
#' @return A list of metadata on a target. Does not include
#'   the file modification time if the target is a file.
#'   That piece is provided later in [make()] by
#'   `drake:::finish_meta`.
#' @param target Character scalar, name of the target
#'   to get metadata.
#' @param config Master internal configuration list produced
#'   by [drake_config()].
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # This example is not really a user-side demonstration.
#' # It just walks through a dive into the internals.
#' # Populate your workspace and write 'report.Rmd'.
#' load_basic_example() # Get the code with drake_example("basic").
#' # Create the master internal configuration list.
#' config <- drake_config(my_plan)
#' # Optionally, compute metadata on 'small',
#' # including a hash/fingerprint
#' # of the dependencies. If meta is not supplied,
#' # drake_build() computes it automatically.
#' meta <- drake_meta(target = "small", config = config)
#' # Should not yet include 'small'.
#' cached()
#' # Build 'small'.
#' # Equivalent to just drake_build(target = "small", config = config).
#' drake_build(target = "small", config = config, meta = meta)
#' # Should now include 'small'
#' cached()
#' readd(small)
#' })
#' }
drake_meta <- function(target, config) {
  meta <- list(
    target = target,
    imported = !(target %in% config$plan$target),
    missing = !target_exists(target = target, config = config),
    seed = seed_from_object(list(seed = config$seed, target = target))
  )
  trigger <- get_trigger(target = target, config = config)
  # Need to make sure meta includes all these
  # fields at the beginning of build_in_hook(),
  # but only after drake decides to actually build the target.
  if (trigger %in% triggers_with_command()){
    meta$command <- get_standardized_command(target = target, config = config)
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
    meta$command <- get_standardized_command(target = target, config = config)
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
  old_hash_exists <- config$cache$exists(key = target, namespace = "kernels")
  if (do_rehash || !old_hash_exists){
    rehash_file(target = target, config = config)
  } else {
    config$cache$get(key = target, namespace = "kernels")
  }
}
