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
  cat("drake_triggers\n")
  str0(x)
}
