#' @title Function migrate_drake_project
#' @export
#' @seealso \code{\link{rescue_cache}}, \code{\link{make}}
#' @param path Full path to the cache
#' @param jobs number of jobs for light parallelism.
#' (Disabled on Windows.)
#' @return \code{TRUE} if the migration was successful, \code{FALSE} otherwise.
#' A migration is successful if the transition preserves target status:
#' that is, outdated targets remain outdated and up to date targets
#' remain up to date.
#' @description Migrate a project/cache from drake 4.4.0 or earlier
#' to be compatible with the version of drake on your system.
#' @details Drake versions after 4.4.0
#' have a different internal structure for the cache.
#' This means projects built with drake 4.4.0 or before are not compatible
#' with projects built with a later version of drake.
#' The \code{migrate_drake_project()} function converts
#' an old cache to a format compatible with the version of drake
#' installed on your system.
#' A migration is successful if the transition preserves target status:
#' that is, outdated targets remain outdated and up to date targets
#' remain up to date. At the end, \code{migrate_drake_project()}
#' tells you whether the migration
#' is successful. If it is not successful,
#' \code{migrate_drake_project()} tells you where
#' it backed up your old project.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # With drake 4.3.0:
#' load_basic_example() # Load drake's canonical example.
#' make(my_plan) # Run the old project.
#' # Now, install drake >= 5.0.0
#' load_basic_example() # Load the canonical example again.
#' make(my_plan) # Error: cache is not back compatible.
#' # Convert the project's '.drake/' cache to the new format.
#' migrate_drake_project()
#' make(my_plan) # Everything is still up to date!
#' # Outdated objects from before migration
#' # should remain out of date afterwards.
#' config <- drake_config(my_plan)
#' outdated(config)
#' })
#' }
migrate_drake_project <- function(
  path = drake::default_cache_path(), jobs = 1
){
  cache <- should_migrate(path = path)
  if (is.null(cache)){
    return(invisible(TRUE))
  }
  version <- drake_session(cache = cache)$otherPkgs$drake$Version # nolint
  backup <- backup_cache_path(path = path, old = version)
  message("Backing up ", path, " to a backup cache at ", backup)
  dir.create(backup)
  file.copy(from = path, to = backup, recursive = TRUE)
  message("Migrating cache at ", path, " for your system's drake.")
  config <- read_drake_config(cache = cache)
  config$cache <- cache
  config$parallelism <- "mclapply"
  config$jobs <- safe_jobs(jobs)
  config$hook <- migrate_hook
  config$envir <- new.env(parent = globalenv())
  config$verbose <- TRUE
  config$trigger <- "any"
  config$execution_graph <- config$graph
  config$store_meta <- TRUE
  config$lazy_load <- FALSE
  config$outdated <- legacy_outdated(config) %>%
    as.character %>%
    sort
  config$cache$clear(namespace = "depends")
  store_drake_config(config = config)
  run_mclapply(config = config)
  message("Checking for outdated targets.")
  config$hook <- empty_hook
  outdated <- outdated(config = config) %>%
    sort
  success <- identical(config$outdated, outdated)
  migration_result(success = success, backup = backup)
  if (success){
    initialize_session(config = config)
  }
  invisible(success)
}

should_migrate <- function(path){
  tryCatch({
      tmp <- this_cache(path = path, force = FALSE, verbose = TRUE)
      if (is.null(tmp)){
        message("No cache found to migrate.")
      } else {
        message(
          "This project is already compatible with your system's drake. ",
          "No need to migrate."
        )
      }
      NULL
    },
    error = function(e){
      this_cache(path = path, force = TRUE, verbose = TRUE)
    }
  )
}

assert_compatible_cache <- function(cache){
  if (is.null(cache)){
    return()
  }
  err <- try(
    old <- drake_session(cache = cache)$otherPkgs$drake$Version, # nolint
    silent = TRUE
  )
  if (inherits(err, "try-error")){
    return(invisible())
  }
  comparison <- compareVersion(old, "4.4.0")
  if (comparison > 0){
    return(invisible())
  }
  current <- packageVersion("drake")
  path <- cache$driver$path
  newpath <- backup_cache_path(path = path, old = old)
  stop(
    "The project at '", path, "' was previously built by drake ", old, ". ",
    "You are running drake ", current, ", which is not back-compatible. ",
    "To format your cache for the newer drake, ",
    "try migrate_drake_project('", path, "'). ",
    "migrate_drake_project() restructures the cache in a way that ",
    "preserves the statuses of your targets (up to date vs outdated). ",
    "But in case of errors, ",
    "migrate_drake_project() first backs up '", path, "' to '",
    newpath, "'. Alternatively, ",
    "you can revert to a back-compatible version ",
    " of drake with 'devtools::install_version(\"drake\", \"4.4.0\")' ",
    " or just run your project from scratch as is with ",
    "make(..., force = TRUE).",
    call. = FALSE
  )
}

backup_cache_path <- function(path, old){
  newpath <- paste0(path, "_backup_drake_", old)
}

migrate_hook <- function(code){
  env <- parent.frame()
  target <- env$target
  config <- env$config
  build_time <- tryCatch(
    config$cache$get(key = target, namespace = "build_times"),
    error = null_proc_time
  )
  config$cache$set(key = target, value = build_time,
    namespace = "build_times")
  value <- tryCatch(
    legacy_readd(target = target, cache = config$cache),
    error = error_na
  )
  meta <- meta(target = target, config = config)
  if (target %in% config$outdated){
    return()
  }
  store_target(target = target, value = value, meta = meta,
    config = config)
}

error_na <- function(e){
  return(NA)
}

null_proc_time <- function(e){
  proc.time() - proc.time()
}

legacy_readd <- function(target, cache){
  store <- cache$get(target)
  if (store$type == "function"){
    value <- cache$get(key = target, namespace = "functions")
  } else{
    value <- store$value
  }
  return(value)
}

migration_result <- function(success, backup){
  if (success){
    migration_success()
  } else {
    migration_failure(backup)
  }
}

migration_failure <- function(backup){
  paste0(
    "migration failed: ",
    "target statuses failed to transfer (outdated vs current). ",
    "Original cache saved: ", backup
  ) %>%
    stop(call. = FALSE)
}

migration_success <- function(){
  message(
    "Migration successful: ",
    "target statuses preserved (outdated vs current)."
  )
}

legacy_outdated <- function(config){
  config$inventory <- config$cache$list()
  config$inventory_filemtime <- config$cache$list(namespace = "filemtime")
  all_targets <- intersect(V(config$graph)$name, config$plan$target)
  hash_list <- hash_list(targets = all_targets, config = config)
  rebuild <- Filter(
    x = all_targets,
    f = function(target){
      hashes <- hash_list[[target]]
      !legacy_target_current(
        target = target, hashes = hashes, config = config)
    }
  )
  if (length(rebuild)){
    lightly_parallelize(
      rebuild,
      function(vertex){
        subcomponent(config$graph, v = vertex, mode = "out")$name
      },
      jobs = config$jobs
    ) %>%
      unlist() %>%
      unique() %>%
      sort()
  }
}

hash_list <- function(targets, config) {
  console_many_targets(targets = targets,
    pattern = "check", config = config)
  out <- lightly_parallelize(
    X = targets, FUN = hashes,
    jobs = config$jobs, config = config
  )
  names(out) <- lapply(out, "[[", "target") %>%
    unlist
  out
}

hashes <- function(target, config) {
  list(
    target = target,
    depends = legacy_dependency_hash(target = target, config = config),
    file = legacy_file_hash(target = target, config = config)
  )
}

legacy_dependency_hash <- function(target, config) {
  command <- legacy_get_command(target = target, config = config)
  stopifnot(length(command) == 1)
  dependencies(target, config) %>%
    legacy_self_hash(config = config) %>%
    c(command) %>%
    digest::digest(algo = config$long_hash_algo)
}

legacy_get_command <- function(target, config){
  config$plan$command[config$plan$target == target] %>% legacy_tidy
}

legacy_tidy <- function(x){
  parse(text = x) %>%
    as.character %>%
    paste(collapse = "\n") %>%
    braces
}

legacy_self_hash <- Vectorize(function(target, config) {
  if (target %in% config$inventory) {
    config$cache$get_hash(target)
  } else {
    as.character(NA)
  }
},
"target", USE.NAMES = FALSE)

legacy_file_hash <- function(target, config, size_cutoff = 1e5) {
  if (is_file(target)) {
    filename <- drake::drake_unquote(target)
  } else {
    return(as.character(NA))
  }
  if (!file.exists(filename))
    return(as.character(NA))
  old_mtime <- ifelse(
    target %in% config$cache$list(namespace = "filemtime"),
    config$cache$get(key = target, namespace = "filemtime"),
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
    out <- config$cache$get(target)
    ifelse(is.character(out), out, out$value)
  }
}

legacy_target_current <- function(target, hashes, config){
  if (!(target %in% config$inventory)){
    return(FALSE)
  }
  if (!legacy_file_current(
    target = target, hashes = hashes, config = config)){
    return(FALSE)
  }
  identical(
    config$cache$get(target, namespace = "depends"),
    hashes$depends
  )
}

legacy_file_current <- function(target, hashes, config){
  if (!is_file(target)){
    return(TRUE)
  }
  if (!file.exists(drake_unquote(target))){
    return(FALSE)
  }
  out <- config$cache$get(target)
  out <- ifelse(is.character(out), out, out$value)
  identical(out, hashes$file)
}
