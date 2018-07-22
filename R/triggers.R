#' @title Set the trigger of a target.
#' @description Use this function inside a target's command
#'   in your [drake_plan()]. The target will rebuild if and only if:
#'   - Any of `command`, `depends`, or `file` is `TRUE`, or
#'   - `condition` evaluates to `TRUE`, or
#'   - `change` evaluates to a value different from last time.
#' @export
#' @seealso [drake_plan()], [make()]
#' @return a list of trigger specification details that
#'   `drake` processes internally when it comes time to decide
#'   whether to build the target.
#' @param condition R code (expression or language object)
#'   that returns a logical. The target will rebuild
#'   if the code evaluates to `TRUE`.
#' @param command logical, whether to rebuild the target if the
#'   [drake_plan()] command changes.
#' @param depends logical, whether to rebuild if a
#'   non-file dependency changes.
#' @param file logical, whether to rebuild the target
#'   if a [file_in()]/[file_out()]/[knitr_in()] file changes.
#' @param change R code (expression or language object)
#'  that returns any value. The target will rebuild
#'   if that value is different from last time
#'   or not already cached.
trigger <- function(
  condition = FALSE,
  command = TRUE,
  depends = TRUE,
  file = TRUE,
  change = NULL
){
  stopifnot(is.logical(command))
  stopifnot(is.logical(depends))
  stopifnot(is.logical(file))
  list(
    condition = rlang::enexpr(condition),
    command = command,
    depends = depends,
    file = file,
    change = rlang::enexpr(change)
  )
}

resolve_trigger <- function(target, config){
  plan <- config$plan
  if (!(target %in% plan$target)){
    trigger <- quote(trigger(condition = TRUE))
  } else {
    trigger <- drake_plan_override(
      target = target,
      field = "trigger",
      config = config
    )
  }
  if (is.character(trigger)) {
    trigger <- parse(text = trigger)
  }
  eval(trigger, envir = config$envir)
}

command_trigger <- function(target, meta, config){
  if (is.null(meta$command)){
    return(FALSE)
  }
  command <- get_from_subspace(
    key = target,
    subspace = "command",
    namespace = "meta",
    cache = config$cache
  )
  !identical(command, meta$command)
}

depends_trigger <- function(target, meta, config){
  if (is.null(meta$dependency_hash)){
    return(FALSE)
  }
  dependency_hash <- get_from_subspace(
    key = target,
    subspace = "dependency_hash",
    namespace = "meta",
    cache = config$cache
  )
  !identical(dependency_hash, meta$dependency_hash)
}

# We really need a file dependency hash just for all the files.
# Maybe that's the role of meta$file
file_trigger <- function(target, meta, config){
  if (!length(target) || !length(config) || !length(meta)){
    return(FALSE)
  }
  for (file in meta$output_files){
    if (!file.exists(drake_unquote(file))){
      return(TRUE)
    }
  }
  for (hash_name in c("input_file_hash", "output_file_hash")){
    old_file_hash <- get_from_subspace(
      key = target,
      subspace = hash_name,
      namespace = "meta",
      cache = config$cache
    )
    if (!identical(old_file_hash, meta[[hash_name]])){
      return(TRUE)
    }
  }
  FALSE
}

change_trigger <- function(target, meta, config){
  if (!length(target) || !length(config) || !length(meta)){
    return(FALSE)
  }
  if (!config$cache$exists(key = target, namespace = "meta")){
    return(TRUE)
  }
  old_trigger <- get_from_subspace(
    key = target,
    subspace = "trigger",
    namespace = "meta",
    cache = config$cache
  )
  !identical(old_trigger$value, meta$trigger$value)
}

should_build_target <- function(target, meta = NULL, config){
  if (is.null(meta)){
    meta <- drake_meta(target = target, config = config)
  }
  if (meta$imported) {
    return(TRUE)
  }
  if (meta$missing){
    return(TRUE)
  }
  if (identical(eval(meta$trigger$condition, envir = config$envir), TRUE)){
    return(TRUE)
  }
  if (identical(meta$trigger$command, TRUE)){
    if (command_trigger(target = target, meta = meta, config = config)){
      return(TRUE)
    }
  }
  if (identical(meta$trigger$depends, TRUE)){
    if (depends_trigger(target = target, meta = meta, config = config)){
      return(TRUE)
    }
  }
  if (identical(meta$trigger$file, TRUE)){
    if (file_trigger(target = target, meta = meta, config = config)){
      return(TRUE)
    }
  }
  if (!is.null(meta$trigger$change)){
    if (change_trigger(target = target, meta = meta, config = config)){
      return(TRUE)
    }
  }
  FALSE
}
