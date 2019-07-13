# Get the command ready for tidy eval prep
# and then pure eval (no side effects).
preprocess_command <- function(command, config) {
  command <- as.call(c(quote(local), command))
  # Here, we really do need expr() instead of quo().
  # `!!` needs to unquote symbols using config$eval instead of
  # the environment where the original binding took place.
  # In other words, `drake` already supplies the correct
  # evaluation environment.
  as.call(c(quote(rlang::expr), command))
}

standardize_command <- function(x) {
  if (is.expression(x) && length(x) == 1L) {
    x <- x[[1]]
  }
  look_for_ignore <- "ignore" %in% all.vars(x, functions = TRUE)
  if (look_for_ignore) {
    x <- ignore_ignore(x)
  }
  attributes(x) <- NULL
  safe_deparse(x)
}

standardize_imported_function <- function(fun) {
  fun <- unwrap_function(fun)
  str <- safe_deparse(fun) # Because the function body still has attributes.
  if (any(grepl("ignore", str, fixed = TRUE))) {
    fun <- ignore_ignore(fun)
    str <- safe_deparse(fun) # Worth it: ignore_ignore is slow.
  }
  gsub("<pointer: 0x[0-9a-zA-Z]*>", "", str)
}
