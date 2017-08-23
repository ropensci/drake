hash_algorithm <- "md5"

hash_list <- function(targets, config) {
  sapply(targets, hashes, config = config, simplify = FALSE,
    USE.NAMES = TRUE)
}

hashes <- function(target, config) {
  list(depends = dependency_hash(target, config), file = file_hash(target,
    config))
}

dependency_hash <- function(target, config) {
  command <- get_command(target = target, config = config)
  stopifnot(length(command) == 1)
  dependencies(target, config) %>% self_hash(config = config) %>%
    c(command) %>% digest(algo = hash_algorithm)
}

self_hash <- Vectorize(function(target, config) {
  if (target %in% config$inventory)
    config$cache$get_hash(target) else as.character(NA)
},
"target", USE.NAMES = FALSE)

should_rehash_file <- function(filename, new_mtime, old_mtime,
  size_cutoff){
  do_rehash <- file.size(filename) < size_cutoff | new_mtime > old_mtime
  if (is.na(do_rehash)){
    do_rehash <- TRUE
  }
  do_rehash
}

file_hash <- function(target, config, size_cutoff = 1e5) {
  if (is_not_file(target))
    return(as.character(NA))
  filename <- eply::unquote(target)
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
    rehash_file(target)
  } else {
    config$cache$get(target)$value
  }
}

rehash_file <- function(target) {
  eply::unquote(target) %>% tools::md5sum() %>% unname
}

tidy <- function(x) {
  parse(text = x) %>% as.character %>%
    paste(collapse = "\n") %>% braces
}

braces <- function(x) {
  paste("{\n", x, "\n}")
}

get_command <- function(target, config) {
  config$plan$command[config$plan$target == target] %>% tidy
}
