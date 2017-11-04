#' @title Function migrate
#' @export
#' @seealso \code{\link{rescue_cache}}, \code{\link{make}}
#' @param path Full path to the cache
#' @param jobs number of jobs for light parallelism.
#' (Disabled on Windows.)
#' @description Migrate a project/cache from drake 4.4.0 or earlier
#' to be compatible with the version of drake on your system.
#' @details Versions after drake have a different internal structure for the cache.
#' This means projects built with drake 4.4.0 or before are not compatible
#' with projects built with a later version of drake. migrate() converts
#' an old cache to a format compatible with the version of drake
#' installed on your system.
migrate <- function(path = drake::default_cache_path(), jobs = 1){
  cache <- should_migrate(path = path)
  if (is.null(cache)){
    return(invisible())
  }
  version <- session(cache = cache)$otherPkgs$drake$Version # nolint
  backup <- backup_cache_path(path = path, old = version)
  cat("Backing up", path, "to a backup cache at ", backup, "\n")
  dir.create(backup)
  file.copy(from = path, to = backup, recursive = TRUE)
  cat("Migrating cache at", path, "for your system's drake.\n")
  config <- read_config(cache = cache)
  config$cache = cache
  config$parallelism = "mclapply"
  config$jobs <- safe_jobs(jobs)
  config$hook <- migrate_hook
  config$envir <- new.env(parent = globalenv())
  config$verbose = TRUE
  config$outdated <- legacy_outdated(config) %>%
    sort
  config$cache$clear(namespace = "depends")
  run_mclapply(config = config)
  cat("Checking for outdated targets.\n")
  config$hook <- function(code){}
  outdated <- outdated(config = config) %>%
    sort
  success <- identical(config$outdated, outdated)
  message <- ifelse(success, migration_success(),
   migration_failure(backup = backup))
  cat(message)
}

should_migrate <- function(path){
  tryCatch({
      tmp <- this_cache(path = path, force = FALSE)
      NULL
    },
    error = function(e){
      this_cache(path = path, force = TRUE)
    }
  )
}

assert_compatible_cache <- function(cache){
  if (is.null(cache)){
    return()
  }
  err <- try(
    old <- session(cache = cache)$otherPkgs$drake$Version, silent = TRUE) # nolint
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
    "To format your cache for the newer drake, try migrate('", path, "'). ",
    "migrate() restructures the cache in a way that ",
    "preserves the statuses of your targets (up to date vs outdated). ",
    "But in case of errors, migrate() first backs up '", path, "' to '",
    newpath, "'. Alternatively, you can just run your project from scratch ",
    "as is with make(..., force = TRUE)."
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
    error = function(e){
      proc.time() - proc.time()
    }
  )
  value <- legacy_readd(target = target, cache = config$cache)
  meta <- meta(target = target, config = config)
  if (target %in% config$outdated){
    return()
  }
  store_target(target = target, value = value, meta = meta,
    build_time = build_time, config = config)
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

migration_failure <- function(backup){
  paste(
    "Migration failed:",
    "target statuses failed to transfer (outdated vs current).",
    "Original cache saved:", backup,
    collapse = "\n"
  )
}

migration_success <- function(){
  paste(
    "Migration successful:",
    "target statuses preserved (outdated vs current).",
    collapse = "\n"
  )
}

legacy_outdated <- function(config){
  config$inventory <- config$cache$list()
  config$inventory_filemtime = config$cache$list(namespace = "filemtime")
  all_targets <- intersect(V(config$graph)$name, config$plan$target)
  hash_list <- hash_list(targets = all_targets, config = config)
  rebuild <- Filter(
    x = all_targets,
    f = function(target){
      hashes <- hash_list[[target]]
      !legacy_target_current(target = target, hashes = hashes, config = config)
    }
  )
  if (!length(rebuild)){
    return(character(0))
  } else{
    lightly_parallelize(
      rebuild,
      function(vertex){
        subcomponent(config$graph, v = vertex, mode = "out")$name
      },
      jobs = jobs
    ) %>%
      unlist() %>%
      unique() %>%
      sort()
  }
}

hash_list <- function(targets, config) {
  console_many_targets(targets = targets,
    message = "check", config = config)
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
  command <- get_command(target = target, config = config)
  stopifnot(length(command) == 1)
  dependencies(target, config) %>%
    legacy_self_hash(config = config) %>%
    c(command) %>%
    digest::digest(algo = config$long_hash_algo)
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
    filename <- eply::unquote(target)
  } else {
    return(as.character(NA))
  }
  if (!file.exists(filename))
    return(as.character(NA))
  old_mtime <- ifelse(target %in% config$inventory_filemtime,
                      config$cache$get(key = target, namespace = "filemtime"),
                      -Inf)
  new_mtime <- file.mtime(filename)
  do_rehash <- should_rehash_file(
    filename = filename,
    new_mtime = new_mtime,
    old_mtime = old_mtime,
    size_cutoff = size_cutoff)
  if (do_rehash){
    rehash_file(target = target, config = config)
  } else {
    config$cache$get(target)$value
  }
}

legacy_target_current <- function(target, hashes, config){
  if (!(target %in% config$inventory)){
    return(FALSE)
  }
  if (!legacy_file_current(target = target, hashes = hashes, config = config)){
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
  if (!file.exists(unquote(target))){
    return(FALSE)
  }
  identical(config$cache$get(target)$value, hashes$file)
}
