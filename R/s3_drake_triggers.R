#' @title Customize the decision rules for rebuilding targets
#' \lifecycle{stable}
#' @description  Use this function inside a target's command
#'   in your [drake_plan()] or the `trigger` argument to
#'   [make()] or [drake_config()].
#'   For details, see the chapter on triggers
#'   in the user manual:
#'   <https://books.ropensci.org/drake/triggers.html>
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
#' @param command Logical, whether to rebuild the target if the
#'   [drake_plan()] command changes.
#' @param depend Logical, whether to rebuild if a
#'   non-file dependency changes.
#' @param file Logical, whether to rebuild the target
#'   if a [file_in()]/[file_out()]/[knitr_in()] file changes.
#'   Also applies to external data tracked with
#'   `target(format = "file")`.
#' @param seed Logical, whether to rebuild the target
#'   if the seed changes. Only makes a difference if you set
#'   a custom `seed` column in your [drake_plan()] at some point
#'   in your workflow.
#' @param format Logical, whether to rebuild the target if the
#'   choice of specialized data format changes: for example,
#'   if you use `target(format = "qs")` one instance and
#'   `target(format = "fst")` the next. See
#'   <https://books.ropensci.org/drake/plans.html#special-data-formats-for-targets> # nolint
#'   for details on formats.
#' @param condition R code (expression or language object)
#'   that returns a logical. The target will rebuild
#'   if the code evaluates to `TRUE`.
#' @param change R code (expression or language object)
#'  that returns any value. The target will rebuild
#'   if that value is different from last time
#'   or not already cached.
#' @param mode A character scalar equal to `"whitelist"` (default) or
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
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
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
#' }
#' })
#' }
trigger <- function(
  command = TRUE,
  depend = TRUE,
  file = TRUE,
  seed = TRUE,
  format = TRUE,
  condition = FALSE,
  change = NULL,
  mode = c("whitelist", "blacklist", "condition")
) {
  command <- as.logical(command)
  depend <- as.logical(depend)
  file <- as.logical(file)
  format <- as.logical(format)
  condition <- rlang::quo_squash(rlang::enquo(condition))
  change <- rlang::quo_squash(rlang::enquo(change))
  mode <- match.arg(mode)
  new_drake_triggers(
    command = command,
    depend = depend,
    file = file,
    seed = seed,
    format = format,
    condition = condition,
    change = change,
    mode = mode
  )
}

#' @title `drake_triggers` helper
#' @keywords internal
#' @description Triggers of a target.
#' @inheritParams trigger
drake_triggers <- trigger

#' @title `drake_triggers` constructor
#' @keywords internal
#' @description List of class `drake_triggers`.
#' @return A `drake_triggers` object.
#' @param command Logical, command trigger.
#' @param depend Logical, depend trigger.
#' @param file Logical, file trigger.
#' @param seed Logical, seed trigger.
#' @param format Logical, format trigger.
#' @param condition Language object or object coercible to logical,
#'   condition trigger.
#' @param change Language object or literal value, change trigger.
#' @param mode Character, mode of condition trigger.
#' @examples
#' if (FALSE) { # stronger than roxygen dontrun
#' new_drake_triggers()
#' }
new_drake_triggers <- function(
  command = TRUE,
  depend = TRUE,
  file = TRUE,
  seed = TRUE,
  format = TRUE,
  condition = FALSE,
  change = NULL,
  mode = "whitelist"
) {
  out <- list(
    command = command,
    depend = depend,
    file = file,
    seed = seed,
    format = format,
    condition = condition,
    change = change,
    mode = mode
  )
  class(out) <- c("drake_triggers", "drake")
  out
}

validate_drake_triggers <- function(x) {
  stopifnot(inherits(x, "drake_triggers"))
  stopifnot(inherits(x, "drake"))
  for (field in c("command", "depend", "file", "seed", "format")) {
    stopifnot(is.logical(x[[field]]))
    stopifnot(length(x[[field]]) == 1L)
  }
  stopifnot(is.logical(x$condition) || is.language(x$condition))
  stopifnot(length(mode) == 1L)
  stopifnot(x$mode %in% c("whitelist", "blacklist", "condition"))
  out_fields <- names(x)
  exp_fields <- c(
    "command",
    "depend",
    "file",
    "seed",
    "format",
    "condition",
    "change",
    "mode"
  )
  stopifnot(identical(out_fields, exp_fields))
}

#' @export
print.drake_triggers <- function(x, ...) {
  message("drake_triggers")
  msg_str(x)
}
