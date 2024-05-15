#' @title List the most upstream *recoverable* outdated targets.
#' `r lifecycle::badge("stable")`
#' @description Only shows the most upstream updated targets.
#'   Whether downstream targets are recoverable depends on
#'   the eventual values of the upstream targets in the next [make()].
#' @section Recovery:
#'  `make(recover = TRUE, recoverable = TRUE)`
#'   powers automated data recovery.
#'   The default of `recover` is `FALSE` because
#'   targets recovered from the distant past may have been generated
#'   with earlier versions of R and earlier package environments
#'   that no longer exist.
#'
#'   How it works: if `recover` is `TRUE`,
#'   `drake` tries to salvage old target values from the cache
#'   instead of running commands from the plan.
#'   A target is recoverable if
#'
#'   1. There is an old value somewhere in the cache that
#'      shares the command, dependencies, etc.
#'      of the target about to be built.
#'   2. The old value was generated with `make(recoverable = TRUE)`.
#'
#'   If both conditions are met, `drake` will
#'
#'   1. Assign the most recently-generated admissible data to the target, and
#'   2. skip the target's command.
#' @export
#' @seealso [r_recoverable()], [r_outdated()], [drake_config()], [missed()],
#'   [drake_plan()], [make()]
#' @return Character vector of the names of recoverable targets.
#' @inheritParams outdated
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan)
#' clean()
#' outdated(my_plan) # Which targets are outdated?
#' recoverable(my_plan) # Which of these are recoverable and upstream?
#' # The report still builds because clean() removes report.md,
#' # but make() recovers the rest.
#' make(my_plan, recover = TRUE)
#' outdated(my_plan)
#' # When was the *recovered* small data actually built (first stored)?
#' # (Was I using a different version of R back then?)
#' diagnose(small)$date
#' # If you set the same seed as before, you can even
#' # rename targets without having to build them again.
#' # For an example, see
#' # the "Reproducible data recovery and renaming" section of
#' # https://github.com/ropensci/drake/blob/main/README.md.
#' }
#' })
#' }
recoverable <- function(
  ...,
  make_imports = TRUE,
  do_prework = TRUE,
  config = NULL
) {
}

#' @title Internal function with a drake_config() argument
#' @export
#' @keywords internal
#' @description Not a user-side function.
#' @inheritParams outdated
#' @param config A [drake_config()] object.
recoverable_impl <- function(
  config = NULL,
  make_imports = TRUE,
  do_prework = TRUE
) {
  assert_config(config)
  if (make_imports && config$settings$lock_cache) {
    config$cache$lock()
    on.exit(config$cache$unlock(), add = TRUE)
  }
  config <- init_config_tmp(config)
  if (do_prework) {
    do_prework(config = config, verbose_packages = config$logger$verbose)
  }
  if (make_imports) {
    process_imports(config = config)
  }
  out <- first_outdated(config)
  index <- vapply(
    out,
    is_recoverable,
    FUN.VALUE = logical(1),
    config = config
  )
  out[index]
}

body(recoverable) <- config_util_body(recoverable_impl)

is_recoverable <- function(target, config) {
  meta <- drake_meta_(target = target, config = config)
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
  meta_old <- drake_meta_old(target, config)
  on.exit(config$meta_old[[target]] <- meta_old)
  config$meta_old[[target]] <- recovery_meta
  meta <- subsume_old_meta(target, meta, config)
  if (any_subtargetlike_triggers(target, meta, config)) {
    return(FALSE)
  }
  if (!is_dynamic(target, config)) {
    return(TRUE)
  }
  all_subtargets_recoverable(target, recovery_meta, config)
}

all_subtargets_recoverable <- function(target, recovery_meta, config) {
  subtargets <- recovery_meta$subtargets
  preregister_subtargets(target, subtargets = subtargets, config)
  ht_set(config$ht_is_subtarget, subtargets)
  is_outdated <- check_subtarget_triggers(target, subtargets, config)
  will_recover <- rep(FALSE, length(subtargets))
  will_recover[is_outdated] <- vapply(
    subtargets[is_outdated],
    is_recoverable,
    FUN.VALUE = logical(1),
    config = config
  )
  all(!is_outdated | will_recover)
}

#' @title List the targets that are out of date.
#' `r lifecycle::badge("stable")`
#' @description Outdated targets will be rebuilt in the next
#'   [make()]. `outdated()` does not show dynamic sub-targets.
#' @export
#' @seealso [r_outdated()], [drake_config()], [missed()], [drake_plan()],
#'   [make()]
#' @return Character vector of the names of outdated targets.
#' @param ... Arguments to [make()], such as `plan` and `targets` and `envir`.
#' @param make_imports Logical, whether to make the imports first.
#'   Set to `FALSE` to save some time and risk obsolete output.
#' @param do_prework Whether to do the `prework`
#'   normally supplied to [make()].
#' @param config Deprecated (2019-12-21).
#'   A configured workflow from [drake_config()].
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # Recopute the config list early and often to have the
#' # most current information. Do not modify the config list by hand.
#' outdated(my_plan) # Which targets are out of date?
#' make(my_plan) # Run the projects, build the targets.
#' # Now, everything should be up to date (no targets listed).
#' outdated(my_plan)
#' }
#' })
#' }
outdated <-  function(
  ...,
  make_imports = TRUE,
  do_prework = TRUE,
  config = NULL
) {
}

#' @title Internal function with a drake_config() argument
#' @export
#' @keywords internal
#' @description Not a user-side function.
#' @inheritParams outdated
#' @param config A [drake_config()] object.
outdated_impl <- function(
  config,
  make_imports = TRUE,
  do_prework = TRUE
) {
  assert_config(config)
  if (!identical(config$running_make, TRUE)) {
    config$logger$file <- NULL
    config <- init_config_tmp(config)
  }
  if (make_imports && config$settings$lock_cache) {
    config$cache$lock()
    on.exit(config$cache$unlock(), add = TRUE)
  }
  if (do_prework) {
    do_prework(config = config, verbose_packages = config$logger$verbose)
  }
  if (make_imports) {
    process_imports(config = config)
  }
  from <- first_outdated(config = config)
  to <- downstream_nodes(config$graph, from)
  out <- sort(unique(as.character(c(from, to))))
  out[!is_encoded_path(out)]
}

body(outdated) <- config_util_body(outdated_impl)

first_outdated <- function(config) {
  config$cache$reset_memo_hash()
  on.exit(config$cache$reset_memo_hash())
  envir <- new.env(parent = emptyenv())
  envir$graph <- subset_graph(config$graph, all_targets(config))
  envir$continue <- TRUE
  while (envir$continue) {
    stage_outdated(envir, config)
  }
  envir$outdated
}

stage_outdated <- function(envir, config) {
  new_leaves <- setdiff(leaf_nodes(envir$graph), envir$outdated)
  do_build <- lightly_parallelize(
    X = new_leaves,
    FUN = is_outdated,
    jobs = config$settings$jobs_preprocess,
    config = config
  )
  do_build <- unlist(do_build)
  envir$outdated <- c(envir$outdated, new_leaves[do_build])
  if (all(do_build)) {
    envir$continue <- FALSE
  } else {
    envir$graph <- delete_vertices(envir$graph, v = new_leaves[!do_build])
  }
}

is_outdated <- function(target, config) {
  if (target_missing(target, config)) {
    return(TRUE)
  }
  class(target) <- ifelse(is_dynamic(target, config), "dynamic", "static")
  config$settings$jobs_preprocess <- 1
  is_outdated_impl(target, config)
}

is_outdated_impl <- function(target, config) {
  UseMethod("is_outdated_impl")
}

#' @export
is_outdated_impl.static <- function(target, config) {
  target <- unclass(target)
  meta <- drake_meta_(target, config)
  any_static_triggers(target, meta, config) ||
    any_subtargetlike_triggers(target, meta, config)
}

#' @export
is_outdated_impl.dynamic <- function(target, config) {
  target <- unclass(target)
  meta <- drake_meta_(target, config)
  meta_old <- drake_meta_old(target, config)
  preregister_subtargets(target, subtargets = meta_old$subtargets, config)
  any_static_triggers(target, meta, config) ||
    check_trigger_dynamic(target, meta, config) ||
    any_subtarget_triggers(target, meta_old$subtargets, config)
}

#' @title Report any import objects required by your drake_plan
#'   plan but missing from your workspace or file system.
#' `r lifecycle::badge("stable")`
#' @description Checks your workspace/environment and
#' file system.
#' @export
#' @seealso [outdated()]
#' @return Character vector of names of missing objects and files.
#' @param ... Arguments to [make()], such as `plan` and `targets`.
#' @param config Deprecated.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' plan <- drake_plan(x = missing::fun(arg))
#' missed(plan)
#' }
#' })
#' }
missed <- function(..., config = NULL) {
}

#' @title Internal function with a drake_config() argument
#' @export
#' @keywords internal
#' @description Not a user-side function.
#' @param config A [drake_config()] object.
missed_impl <- function(config) {
  assert_config(config)
  imports <- all_imports(config)
  is_missing <- lightly_parallelize(
    X = imports,
    FUN = missing_import,
    jobs = config$settings$jobs_preprocess,
    config = config
  )
  is_missing <- as.logical(is_missing)
  if (!any(is_missing)) {
    return(character(0))
  }
  config$cache$display_keys(imports[is_missing])
}

body(missed) <- config_util_body(missed_impl)

missing_import <- function(x, config) {
  if (is_encoded_path(x)) {
    return(!file_dep_exists(config$cache$decode_path(x)))
  }
  identical(get_import_from_memory(x, config = config), NA_character_)
}

ht_target_exists <- function(config) {
  keys_data <- config$cache$list()
  keys_meta <- config$cache$list(namespace = "meta")
  keys <- intersect(keys_data, keys_meta)
  ht_new(keys, hash = TRUE)
}
