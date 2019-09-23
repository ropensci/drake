#' @title List the most upstream *recoverable* outdated targets.
#' \lifecycle{experimental}
#' @description Only shows the most upstream updated targets.
#'   Whether downstream targets are recoverable depends on
#'   the eventual values of the upstream targets in the next [make()].
#' @section Recovery:
#'  `make(recover = TRUE, recoverable = TRUE)`
#'   powers automated data recovery.
#'   The default of `recover` is `FALSE` because
#'
#'   1. Automated data recovery is still experimental.
#'   2. It has reproducibility issues.
#'   Targets recovered from the distant past may have been generated
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
#' config <- drake_config(my_plan)
#' make(my_plan)
#' clean()
#' outdated(config) # Which targets are outdated?
#' recoverable(config) # Which of these are recoverable and upstream?
#' # The report still builds because clean() removes report.md,
#' # but make() recovers the rest.
#' make(my_plan, recover = TRUE)
#' # When was the *recovered* small data actually built (first stored)?
#' # (Was I using a different version of R back then?)
#' diagnose(small)$date
#' # If you set the same seed as before, you can even
#' # rename targets without having to build them again.
#' # For an example, see
#' # the "Reproducible data recovery and renaming" section of
#' # https://github.com/ropensci/drake/blob/master/README.md.
#' }
#' })
#' }
recoverable <-  function(
  config,
  make_imports = TRUE,
  do_prework = TRUE
) {
  config$logger$minor("begin recoverable()")
  on.exit(config$logger$minor("end recoverable()"), add = TRUE)
  assert_config_not_plan(config)
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

is_recoverable <- function(target, config) {
  meta <- drake_meta_(target = target, config = config)
  key <- recovery_key(target = target, meta = meta, config = config)
  if (!config$cache$exists(key, namespace = "recover")) {
    return(FALSE)
  }
  meta_hash <- config$cache$get_hash(key, namespace = "recover")
  recovery_meta <- config$cache$driver$get_object(meta_hash)
  value_hash <- recovery_meta$hash
  config$cache$exists_object(meta_hash) &&
    config$cache$exists_object(value_hash)
}

#' @title List the targets that are out of date.
#' \lifecycle{stable}
#' @description Outdated targets will be rebuilt in the next
#'   [make()].
#' @export
#' @seealso [r_outdated()], [drake_config()], [missed()], [drake_plan()],
#'   [make()]
#' @return Character vector of the names of outdated targets.
#' @param config Optional internal runtime parameter list
#'   produced with [drake_config()].
#'   You must use a fresh `config` argument with an up-to-date
#'   dependency graph that was never modified by hand.
#'   If needed, rerun [drake_config()] early and often.
#'   See the details in the help file for [drake_config()].
#' @param make_imports Logical, whether to make the imports first.
#'   Set to `FALSE` to save some time and risk obsolete output.
#' @param do_prework Whether to do the `prework`
#'   normally supplied to [make()].
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # Recopute the config list early and often to have the
#' # most current information. Do not modify the config list by hand.
#' config <- drake_config(my_plan)
#' outdated(config = config) # Which targets are out of date?
#' make(my_plan) # Run the projects, build the targets.
#' config <- drake_config(my_plan)
#' # Now, everything should be up to date (no targets listed).
#' outdated(config = config)
#' }
#' })
#' }
outdated <-  function(
  config,
  make_imports = TRUE,
  do_prework = TRUE
) {
  config$logger$minor("begin outdated()")
  on.exit(config$logger$minor("end outdated()"), add = TRUE)
  assert_config_not_plan(config)
  if (do_prework) {
    do_prework(config = config, verbose_packages = config$logger$verbose)
  }
  if (make_imports) {
    process_imports(config = config)
  }
  from <- first_outdated(config = config)
  config$logger$minor("find downstream outdated targets")
  to <- downstream_nodes(config$graph, from)
  out <- sort(unique(as.character(c(from, to))))
  out[!is_encoded_path(out)]
}

first_outdated <- function(config) {
  config$cache$reset_memo_hash()
  on.exit(config$cache$reset_memo_hash())
  out <- character(0)
  old_leaves <- NULL
  config$graph <- subset_graph(config$graph, all_targets(config))
  while (TRUE) {
    config$logger$minor("find more outdated targets")
    new_leaves <- setdiff(leaf_nodes(config$graph), out)
    do_build <- lightly_parallelize(
      X = new_leaves,
      FUN = function(target) {
        if (!config$cache$exists(key = target)) {
          return(TRUE)
        }
        meta <- drake_meta_(target, config)
        sense_trigger(target, meta, config)
      },
      jobs = config$jobs_preprocess
    )
    do_build <- unlist(do_build)
    out <- c(out, new_leaves[do_build])
    if (all(do_build)) {
      break
    } else {
      config$graph <- delete_vertices(config$graph, v = new_leaves[!do_build])
    }
    old_leaves <- new_leaves
  }
  out
}

#' @title Report any import objects required by your drake_plan
#'   plan but missing from your workspace or file system.
#' \lifecycle{stable}
#' @description Checks your workspace/environment and
#' file system.
#' @export
#' @seealso [outdated()]
#' @return Character vector of names of missing objects and files.
#'
#' @param config Internal runtime parameter list
#'   produced by both [drake_config()] and [make()].
#'
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- drake_config(my_plan)
#' missed(config) # All the imported files and objects should be present.
#' rm(reg1) # Remove an import dependency from you workspace.
#' missed(config) # Should report that reg1 is missing.
#' }
#' })
#' }
missed <- function(config) {
  config$logger$minor("begin missed()")
  on.exit(config$logger$minor("end missed()"), add = TRUE)
  assert_config_not_plan(config)
  imports <- all_imports(config)
  is_missing <- lightly_parallelize(
    X = imports,
    FUN = function(x) {
      missing_import(x, config = config)
    },
    jobs = config$jobs_preprocess
  )
  is_missing <- as.logical(is_missing)
  if (!any(is_missing)) {
    return(character(0))
  }
  config$cache$display_keys(imports[is_missing])
}

missing_import <- function(x, config) {
  if (is_encoded_path(x)) {
    return(!file_dep_exists(config$cache$decode_path(x)))
  }
  identical(get_import_from_memory(x, config = config), NA_character_)
}
