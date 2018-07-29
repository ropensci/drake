#' @title Set the trigger of a target.
#' @description For details, see the chapter on triggers
#'   in the user manual:
#'   <https://ropenscilabs.github.io/drake-manual>
#' @details Use this function inside a target's command
#'   in your [drake_plan()]. The target will rebuild if and only if:
#'   - Any of `command`, `depend`, or `file` is `TRUE`, or
#'   - `condition` evaluates to `TRUE`, or
#'   - `change` evaluates to a value different from last time.
#'   There may be a slight efficiency loss if you set complex
#'   triggers for `change` and/or `condition` because
#'   `drake` needs to load any required dependencies
#'   into memory before evaluating these triggers.
#' @export
#' @seealso [drake_plan()], [make()]
#' @return a list of trigger specification details that
#'   `drake` processes internally when it comes time to decide
#'   whether to build the target.
#' @param command logical, whether to rebuild the target if the
#'   [drake_plan()] command changes.
#' @param depend logical, whether to rebuild if a
#'   non-file dependency changes.
#' @param file logical, whether to rebuild the target
#'   if a [file_in()]/[file_out()]/[knitr_in()] file changes.
#' @param condition R code (expression or language object)
#'   that returns a logical. The target will rebuild
#'   if the code evaluates to `TRUE`.
#' @param change R code (expression or language object)
#'  that returns any value. The target will rebuild
#'   if that value is different from last time
#'   or not already cached.
#' @examples
#' # A trigger is just a set of decision rules
#' # to decide whether to build a target.
#' trigger()
#' # This trigger will build a target on Tuesdays
#' # and when the value of an online dataset changes.
#' trigger(condition = today() == "Tuesday", change = get_online_dataset())
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # You can use a global trigger argument:
#' # for example, to always run everything.
#' make(my_plan, trigger = trigger(condition = TRUE))
#' make(my_plan, trigger = trigger(condition = TRUE))
#' # You can also define specific triggers for each target.
#' plan <- drake_plan(
#'   x = rnorm(15),
#'   y = target(
#'     command = x + 1,
#'     trigger = trigger(depend = FALSE)
#'   )
#' )
#' # Now, when x changes, y will not.
#' make(plan)
#' make(plan)
#' plan$command[1] <- "rnorm(16)" # change x
#' make(plan)
#' })
#' }
trigger <- function(
  command = TRUE,
  depend = TRUE,
  file = TRUE,
  condition = NULL,
  change = NULL
){
  stopifnot(is.logical(command))
  stopifnot(is.logical(depend))
  stopifnot(is.logical(file))
  list(
    command = command,
    depend = depend,
    file = file,
    condition = rlang::enexpr(condition),
    change = rlang::enexpr(change)
  )
}

parse_trigger <- function(trigger, envir){
 if (is.character(trigger)) {
    trigger <- convert_old_trigger(trigger)
    trigger <- parse(text = trigger)
  }
  eval(trigger, envir = envir)
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

depend_trigger <- function(target, meta, config){
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
  file_out <- vertex_attr(
    graph = config$graph,
    name = "deps",
    index = target
  )[[1]]$file_out
  for (file in file_out){
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
  if (!config$cache$exists(key = target, namespace = "change")){
    return(FALSE) # nocov
  }
  old_value <- config$cache$get(key = target, namespace = "change")
  !identical(old_value, meta$trigger$value)
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
  if (identical(meta$trigger$condition, TRUE)){
    return(TRUE)
  }
  if (identical(meta$trigger$command, TRUE)){
    if (command_trigger(target = target, meta = meta, config = config)){
      return(TRUE)
    }
  }
  if (identical(meta$trigger$depend, TRUE)){
    if (depend_trigger(target = target, meta = meta, config = config)){
      return(TRUE)
    }
  }
  if (identical(meta$trigger$file, TRUE)){
    if (file_trigger(target = target, meta = meta, config = config)){
      return(TRUE)
    }
  }
  if (!is.null(meta$trigger$condition) && !is.logical(meta$trigger$condition)){
    vertex_attr(
      graph = config$graph,
      name = "deps",
      index = target
    )[[1]]$condition %>%
      ensure_loaded(config = config)
    if (identical(eval(meta$trigger$condition, envir = config$envir), TRUE)){
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
