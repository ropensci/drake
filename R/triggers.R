#' @title Customize the decision rules for rebuilding targets
#' @description  Use this function inside a target's command
#'   in your [drake_plan()] or the `trigger` argument to
#'   [make()] or [drake_config()].
#'   For details, see the chapter on triggers
#'   in the user manual:
#'   <https://ropenscilabs.github.io/drake-manual>
#' @details
#'   A target always builds if it has not been built before.
#'   Triggers allow you to customize the conditions
#'   under which a pre-existing target *re*builds.
#'   By default, the target will rebuild if and only if:
#'   - Any of `command`, `depend`, or `file` is `TRUE`, or
#'   - `condition` evaluates to `TRUE`, or
#'   - `change` evaluates to a value different from last time.
#'   The above steps correspond to the "whitelist" decision rule.
#'   You can select other decision rules with the `mode` argument
#'   described in this help file.
#'   On another note, there may be a slight efficiency loss
#'   if you set complex triggers
#'   for `change` and/or `condition` because
#'   `drake` needs to load any required dependencies
#'   into memory before evaluating these triggers.
#' @export
#' @seealso [drake_plan()], [make()]
#' @return A list of trigger specification details that
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
#' @param mode a character scalar equal to `"whitelist"` (default) or
#'   `"blacklist"` or `"condition"`. With the `mode` argument, you can choose
#'   how the `condition` trigger factors into the decision to build
#'   or skip the target. Here are the options.
#'   - `"whitelist"` (default): we *rebuild* the target whenever `condition`
#'     evaluates to `TRUE`. Otherwise, we defer to the other triggers.
#'     This behavior is the same as the decision rule described in the
#'     "Details" section of this help file.
#'   - `"blacklist"`: we *skip* the target whenever `condition` evaluates
#'     to `FALSE`. Otherwise, we defer to the other triggers.
#'   - `"condition"`: here, the `condition` trigger is the only decider,
#'     and we ignore all the other triggers. We *rebuild* target whenever
#'     `condition` evaluates to `TRUE` and *skip* it whenever `condition`
#'     evaluates to `FALSE`.
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
#'   x = sample.int(15),
#'   y = target(
#'     command = x + 1,
#'     trigger = trigger(depend = FALSE)
#'   )
#' )
#' # Now, when x changes, y will not.
#' make(plan)
#' make(plan)
#' plan$command[1] <- "sample.int(16)" # change x
#' make(plan)
#' })
#' }
trigger <- function(
  command = TRUE,
  depend = TRUE,
  file = TRUE,
  condition = FALSE,
  change = NULL,
  mode = c("whitelist", "blacklist", "condition")
) {
  stopifnot(is.logical(command))
  stopifnot(is.logical(depend))
  stopifnot(is.logical(file))
  list(
    command = command,
    depend = depend,
    file = file,
    condition = rlang::enexpr(condition),
    change = rlang::enexpr(change),
    mode = match.arg(mode)
  )
}

parse_trigger <- function(trigger, envir) {
 if (is.character(trigger)) {
    trigger <- convert_old_trigger(trigger)
    trigger <- parse(text = trigger)
  }
  eval(trigger, envir = envir)
}

command_trigger <- function(target, meta, config) {
  if (is.null(meta$command)) {
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

depend_trigger <- function(target, meta, config) {
  if (is.null(meta$dependency_hash)) {
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
file_trigger <- function(target, meta, config) {
  if (!length(target) || !length(config) || !length(meta)) {
    return(FALSE)
  }
  file_out <- config$layout[[target]]$deps_build$file_out
  for (file in file_out) {
    if (!file.exists(decode_path(file, config))) {
      return(TRUE)
    }
  }
  for (hash_name in c("input_file_hash", "output_file_hash")) {
    old_file_hash <- get_from_subspace(
      key = target,
      subspace = hash_name,
      namespace = "meta",
      cache = config$cache
    )
    if (!identical(old_file_hash, meta[[hash_name]])) {
      return(TRUE)
    }
  }
  FALSE
}

condition_trigger <- function(target, meta, config) {
  if (!length(target) || !length(config) || !length(meta)) {
    return(FALSE)
  }
  if (is.language(meta$trigger$condition)) {
    deps <- config$layout[[target]]$deps_condition
    deps <- deps[c("globals", "namespaced", "loadd", "readd")]
    deps <- ensure_loaded(unlist(deps), config = config)
    value <- eval(meta$trigger$condition, envir = config$eval)
    value <- as.logical(value)
  } else {
    value <- as.logical(meta$trigger$condition)
  }
  if (length(value) != 1 || !is.logical(value)) {
    drake_error(
      "The `condition` trigger must evaluate to a logical of length 1. ",
      "got `", value, "` for target ", target, ".",
      config = config
    )
  }
  condition_decision(value = value, mode = meta$trigger$mode)
}

condition_decision <- function(value, mode) {
  if (identical(mode, "whitelist") && identical(value, TRUE)) {
    return(TRUE)
  }
  if (identical(mode, "blacklist") && identical(value, FALSE)) {
    return(FALSE)
  }
  if (identical(mode, "condition")) {
    return(value)
  }
  "defer"
}

change_trigger <- function(target, meta, config) {
  if (!length(target) || !length(config) || !length(meta)) {
    return(FALSE)
  }
  if (!config$cache$exists(key = target, namespace = "change")) {
    return(FALSE) # nocov
  }
  old_value <- config$cache$get(key = target, namespace = "change")
  !identical(old_value, meta$trigger$value)
}

should_build_target <- function(target, meta = NULL, config) {
  if (is.null(meta)) {
    meta <- drake_meta(target = target, config = config)
  }
  if (meta$imported) {
    return(TRUE)
  }
  if (meta$missing) {
    return(TRUE)
  }
  condition <- condition_trigger(target = target, meta = meta, config = config)
  if (is.logical(condition)) {
    return(condition)
  }
  if (identical(meta$trigger$command, TRUE)) {
    if (command_trigger(target = target, meta = meta, config = config)) {
      return(TRUE)
    }
  }
  if (identical(meta$trigger$depend, TRUE)) {
    if (depend_trigger(target = target, meta = meta, config = config)) {
      return(TRUE)
    }
  }
  if (identical(meta$trigger$file, TRUE)) {
    if (file_trigger(target = target, meta = meta, config = config)) {
      return(TRUE)
    }
  }
  if (!is.null(meta$trigger$change)) {
    if (change_trigger(target = target, meta = meta, config = config)) {
      return(TRUE)
    }
  }
  FALSE
}
