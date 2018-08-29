#' @title Compute the initial pre-build metadata of a target or import.
#' @description The metadata helps determine if the
#' target is up to date or outdated. The metadata of imports
#' is used to compute the metadata of targets.
#' @details Target metadata is computed
#' with `drake_meta()`, and then
#' `drake:::store_outputs()` completes the metadata
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
#'   That piece is computed later in [make()] by
#'   `drake:::store_outputs()`.
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
#' load_mtcars_example() # Get the code with drake_example("mtcars").
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
drake_meta <- function(target, config = drake::read_drake_config()) {
  deps <- vertex_attr(
    graph = config$graph,
    name = "deps",
    index = target
  )[[1]]
  meta <- list(
    name = target,
    target = target,
    imported = !(target %in% config$plan$target),
    foreign = !exists(x = target, envir = config$envir, inherits = FALSE),
    missing = !target_exists(target = target, config = config),
    seed = seed_from_basic_types(config$seed, target)
  )
  # For imported files.
  if (is_file(target)) {
    meta$mtime <- file.mtime(drake::drake_unquote(target))
  }
  if (meta$imported){
    meta$trigger <- trigger(condition = TRUE)
  } else {
    meta$trigger <- vertex_attr(
      graph = config$graph,
      name = "trigger",
      index = target
    )[[1]] %>%
      as.list
  }
  # Need to make sure meta includes all these
  # fields at the beginning of build_in_hook(),
  # but only after drake decides to actually build the target.
  if (meta$trigger$command){
    meta$command <- get_standardized_command(target = target, config = config)
  }
  if (meta$trigger$depend){
    meta$dependency_hash <- dependency_hash(target = target, config = config)
  }
  if (meta$trigger$file){
    meta$input_file_hash <- input_file_hash(target = target, config = config)
    meta$output_file_hash <- output_file_hash(target = target, config = config)
  }
  if (!is.null(meta$trigger$change)){
    vertex_attr(
      graph = config$graph,
      name = "deps",
      index = target
    )[[1]]$change %>%
      ensure_loaded(config = config)
    meta$trigger$value <- eval(meta$trigger$change, config$envir)
  }
  meta
}

dependency_hash <- function(target, config) {
  x <- vertex_attr(
    graph = config$graph,
    name = "deps",
    index = target
  )[[1]]
  deps <- c(x$globals, x$namespaced, x$loadd, x$readd)
  if (!(target %in% config$plan$target)){
    deps <- c(deps, x$file_in, x$knitr_in)
  }
  sort(unique(deps)) %>%
    self_hash(config = config)
}

input_file_hash <- function(
  target,
  config,
  size_cutoff = rehash_file_size_cutoff
){
  deps <- vertex_attr(
    graph = config$graph,
    name = "deps",
    index = target
  )[[1]]
  files <- sort(unique(as.character(c(deps$file_in, deps$knitr_in))))
  vapply(
    X = files,
    FUN = file_hash,
    FUN.VALUE = character(1),
    config = config,
    size_cutoff = size_cutoff
  )
}

output_file_hash <- function(
  target,
  config,
  size_cutoff = rehash_file_size_cutoff
){
  deps <- vertex_attr(
    graph = config$graph,
    name = "deps",
    index = target
  )[[1]]
  files <- sort(unique(as.character(deps$file_out)))
  vapply(
    X = files,
    FUN = file_hash,
    FUN.VALUE = character(1),
    config = config,
    size_cutoff = size_cutoff
  )
}

self_hash <- Vectorize(function(target, config) {
  if (kernel_exists(target = target, config = config)) {
    config$cache$get_hash(target, namespace = "kernels")
  } else {
    as.character(NA)
  }
},
"target", USE.NAMES = TRUE)

rehash_file <- function(target, config) {
  file <- drake::drake_unquote(target)
  if (!file.exists(file)){
    return(as.character(NA))
  }
  digest::digest(
    object = file,
    algo = config$long_hash_algo,
    file = TRUE,
    serialize = FALSE
  )
}

safe_rehash_file <- function(target, config){
  if (file.exists(drake_unquote(target))){
    rehash_file(target = target, config = config)
  } else {
    as.character(NA)
  }
}

should_rehash_file <- function(filename, new_mtime, old_mtime,
  size_cutoff){
  do_rehash <- file.size(filename) < size_cutoff | new_mtime > old_mtime
  if (is.na(do_rehash)){
    do_rehash <- TRUE
  }
  do_rehash
}

file_hash <- function(
  target, config, size_cutoff = rehash_file_size_cutoff) {
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
