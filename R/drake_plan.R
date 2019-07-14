#' @title Create a workflow plan data frame
#'   for the `plan` argument of [make()].
#' @description A `drake` plan is a data frame with columns
#'   `"target"` and `"command"`. Each target is an R object
#'   produced in your workflow, and each command is the
#'   R code to produce it. The `"target"` column has the names
#'   of the targets, and no duplicate elements are allowed.
#'   The `"command"` column is either a character vector of
#'   code strings or a list of language objects.
#'
#' @details `drake` has special syntax for generating large plans.
#'  Your code will look something like
#'  `drake_plan(x = target(cmd, transform = f(y, z), group = g)`
#'  where `f()` is either `map()`, `cross()`, `split()`, or `combine()`
#'  (similar to `purrr::pmap()`, `tidy::crossing()`, `base::split()`,
#'  and  `dplyr::summarize()`, respectively).
#'  These verbs mimic Tidyverse behavior to scale up
#'  existing plans to large numbers of targets.
#'  You can read about this interface at
#'  <https://ropenscilabs.github.io/drake-manual/plans.html#create-large-plans-the-easy-way>. # nolint
#'
#' There is also special syntax for declaring input files,
#'  output files, and knitr reports so dependencies are
#'  properly accounted for ([file_in()], [file_out()], and
#'  [knitr_in()], respectively.
#'
#' Besides `"target"` and `"command"`, you may include
#'   optional columns in your workflow plan. For details, visit
#'   <https://ropenscilabs.github.io/drake-manual/plans.html#special-custom-columns-in-your-plan>
#'
#' @export
#' @return A data frame of targets, commands, and optional
#'   custom columns.
#' @inheritParams transform_plan
#' @param ... A collection of symbols/targets
#'   with commands assigned to them. See the examples for details.
#' @param list Deprecated
#' @param file_targets Deprecated.
#' @param strings_in_dots Deprecated.
#' @param tidy_evaluation Deprecated. Use `tidy_eval` instead.
#' @param transform Logical, whether to transform the plan
#'   into a larger plan with more targets.
#'   Requires the `transform` field in
#'   `target()`. See the examples for details.
#' @examples
#' isolate_example("Contain side effects", {
#' # Create workflow plan data frames.
#' mtcars_plan <- drake_plan(
#'   write.csv(mtcars[, c("mpg", "cyl")], file_out("mtcars.csv")),
#'   value = read.csv(file_in("mtcars.csv"))
#' )
#' mtcars_plan
#' make(mtcars_plan) # Makes `mtcars.csv` and then `value`
#' head(readd(value))
#' # You can use knitr inputs too. See the top command below.
#' load_mtcars_example()
#' head(my_plan)
#' # The `knitr_in("report.Rmd")` tells `drake` to dive into the active
#' # code chunks to find dependencies.
#' # There, `drake` sees that `small`, `large`, and `coef_regression2_small`
#' # are loaded in with calls to `loadd()` and `readd()`.
#' deps_code("report.Rmd")
#'
#' # Use transformations to generate large plans.
#' # Read more at
#' # <https://ropenscilabs.github.io/drake-manual/plans.html#create-large-plans-the-easy-way>. # nolint
#' drake_plan(
#'   data = target(
#'     simulate(nrows),
#'     transform = map(nrows = c(48, 64)),
#'     custom_column = 123
#'   ),
#'   reg = target(
#'     reg_fun(data),
#'    transform = cross(reg_fun = c(reg1, reg2), data)
#'   ),
#'   summ = target(
#'     sum_fun(data, reg),
#'    transform = cross(sum_fun = c(coef, residuals), reg)
#'   ),
#'   winners = target(
#'     min(summ),
#'     transform = combine(summ, .by = c(data, sum_fun))
#'   )
#' )
#'
#' # Split data among multiple targets.
#' drake_plan(
#'   large_data = get_data(),
#'   slice_analysis = target(
#'     large_data %>%
#'       analyze(),
#'     transform = split(large_data, slices = 4)
#'   ),
#'   results = target(
#'     rbind(slice_analysis),
#'     transform = combine(slice_analysis)
#'   )
#' )
#'
#' # Set trace = TRUE to show what happened during the transformation process.
#' drake_plan(
#'   data = target(
#'     simulate(nrows),
#'     transform = map(nrows = c(48, 64)),
#'     custom_column = 123
#'   ),
#'   reg = target(
#'     reg_fun(data),
#'    transform = cross(reg_fun = c(reg1, reg2), data)
#'   ),
#'   summ = target(
#'     sum_fun(data, reg),
#'    transform = cross(sum_fun = c(coef, residuals), reg)
#'   ),
#'   winners = target(
#'     min(summ),
#'     transform = combine(summ, .by = c(data, sum_fun))
#'   ),
#'   trace = TRUE
#' )
#'
#' # You can create your own custom columns too.
#' # See ?triggers for more on triggers.
#' drake_plan(
#'   website_data = target(
#'     command = download_data("www.your_url.com"),
#'     trigger = "always",
#'     custom_column = 5
#'   ),
#'   analysis = analyze(website_data)
#' )
#'
#' # Tidy evaluation can help generate super large plans.
#' sms <- rlang::syms(letters) # To sub in character args, skip this.
#' drake_plan(x = target(f(char), transform = map(char = !!sms)))
#' })
drake_plan <- function(
  ...,
  list = character(0),
  file_targets = NULL,
  strings_in_dots = NULL,
  tidy_evaluation = NULL,
  transform = TRUE,
  trace = FALSE,
  envir = parent.frame(),
  tidy_eval = TRUE,
  max_expand = NULL
) {
  if (length(file_targets) || length(strings_in_dots)) {
    # 2019-02-01 nolint
    warning(
      "Arguments `file_targets` and `strings_in_dots` ",
      "of `drake_plan()` are deprecated.",
      call. = FALSE
    )
  }
  if (length(list)) {
    # 2019-02-01 nolint
    warning(
      "The `list` argument of `drake_plan()` is deprecated. ",
      "Use the interface described at ",
      "https://ropenscilabs.github.io/drake-manual/plans.html#large-plans."
    )
  }
  if (!is.null(tidy_evaluation)) {
    # 2019-04-02 nolint
    warning(
      "The `tidy_evaluation` argument of `drake_plan()` is deprecated. ",
      "Use the `tidy_eval` argument instead."
    )
    tidy_eval <- tidy_evaluation
  }
  force(envir)
  dots <- match.call(expand.dots = FALSE)$...
  warn_arrows(dots)
  list <- lapply(list, function(x) parse(text = x))
  commands <- c(dots, list)
  commands <- select_valid_lang(commands)
  if (!length(commands)) {
    return(empty_plan())
  }
  commands <- complete_target_names(commands)
  targets <- names(commands)
  plan <- weak_tibble(target = targets)
  plan$command <- commands
  plan <- parse_custom_plan_columns(plan)
  if (transform && ("transform" %in% colnames(plan))) {
    plan <- transform_plan_(
      plan = plan,
      envir = envir,
      trace = trace,
      max_expand = max_expand,
      tidy_eval = FALSE,
      sanitize = FALSE
    )
  }
  if (tidy_eval) {
    for (col in setdiff(colnames(plan), c("target", "transform"))) {
      plan[[col]] <- tidyeval_exprs(plan[[col]], envir = envir)
    }
  }
  sanitize_plan(plan)
}

parse_custom_plan_columns <- function(plan) {
  Sys.setenv("drake_target_silent" = "true")
  on.exit(Sys.setenv("drake_target_silent" = ""))
  splits <- split(plan, seq_len(nrow(plan)))
  out <- lapply(splits, parse_custom_plan_row)
  out <- do.call(drake_bind_rows, out)
  sanitize_plan(out)
}

parse_custom_plan_row <- function(row) {
  expr <- row$command
  if (!length(expr) || !is_target_call(expr[[1]])) {
    return(row)
  }
  out <- eval(expr[[1]])
  out$target <- row$target
  out
}

is_target_call <- function(expr) {
  tryCatch(
    safe_deparse(expr[[1]]) %in% target_fns,
    error = error_false
  )
}

#' @title Define custom columns in a [drake_plan()].
#' @description Not a user-side function. Please use from within
#'   [drake_plan()] only.
#' @export
#' @keywords internal
#' @seealso [drake_plan()], [make()]
#' @return A one-row workflow plan data frame with the named
#' arguments as columns.
#' @param command The command to build the target.
#' @param ... Named arguments specifying non-standard
#'   fields of the workflow plan.
#' @examples
#' # Use target() to create your own custom columns in a drake plan.
#' # See ?triggers for more on triggers.
#' drake_plan(
#'   website_data = target(
#'     download_data("www.your_url.com"),
#'     trigger = "always",
#'     custom_column = 5
#'   ),
#'   analysis = analyze(website_data)
#' )
target <- function(command = NULL, ...) {
  # TODO: remove this warning when we unexport target().
  if (!nzchar(Sys.getenv("drake_target_silent"))) {
    .Deprecated(
      "target",
      package = "drake",
      msg = paste(
        "target() is deprecated as a user-side function.",
        "Use target from inside drake_plan(). See",
        "https://ropenscilabs.github.io/drake-manual/plans.html#large-plans",
        "for details."
      )
    )
  }
  call <- match.call(expand.dots = FALSE)
  lst <- call$...
  lst <- select_nonempty(lst)
  lst <- lst[nzchar(names(lst))]
  lst <- c(command = call$command, lst)
  lst <- lapply(lst, function(x) {
    if (is.language(x)) x <- list(x)
    x
  })
  out <- data.frame(command = NA, stringsAsFactors = FALSE)
  for (col in names(lst)) {
    out[[col]] <- lst[[col]]
  }
  out
}

drake_bind_rows <- function(...) {
  args <- rlang::dots_list(..., .ignore_empty = "all")
  df_env <- new.env(parent = emptyenv())
  df_env$dfs <- list()
  flatten_df_list(args, df_env = df_env)
  dfs <- df_env$dfs
  cols <- lapply(dfs, colnames)
  cols <- Reduce(f = union, x = cols)
  dfs <- lapply(dfs, fill_cols, cols = cols)
  do.call(rbind, dfs)
}

flatten_df_list <- function(args, df_env){
  if (!is.null(dim(args))) {
    index <- length(df_env$dfs) + 1
    df_env$dfs[[index]] <- weak_as_tibble(args)
  } else {
    lapply(args, flatten_df_list, df_env = df_env)
  }
}

fill_cols <- function(x, cols) {
  for (col in setdiff(cols, colnames(x))) {
    x[[col]] <- rep(NA, nrow(x))
  }
  x
}

sanitize_plan <- function(plan, allow_duplicated_targets = FALSE) {
  fields <- intersect(colnames(plan), c("command", "target", "trigger"))
  for (field in fields) {
    if (!is.null(plan[[field]])) {
      plan[[field]] <- factor_to_character(plan[[field]])
      if (is.character(plan[[field]])) {
        plan[[field]] <- trimws(plan[[field]])
      }
    }
  }
  plan$target <- make.names(plan$target, unique = FALSE, allow_ = TRUE)
  plan <- plan[nzchar(plan$target), ]
  first <- c("target", "command")
  cols <- c(first, setdiff(colnames(plan), first))
  if (!allow_duplicated_targets) {
    plan <- assert_unique_targets(plan[, cols])
  }
  plan <- arrange_plan_cols(plan)
  for (col in lang_cols(plan)) {
    if (!is.list(plan[[col]])) {
      plan[[col]] <- lapply(plan[[col]], safe_parse)
    }
  }
  for (col in setdiff(colnames(plan), c("target", "command", "trigger"))) {
    plan[[col]] <- unlist(plan[[col]])
  }
  as_drake_plan(plan)
}

assert_unique_targets <- function(plan) {
  dups <- duplicated(plan$target)
  if (any(dups)) {
    stop(
      "duplicated target names:\n",
      multiline_message(plan$target[dups]),
      call. = FALSE
    )
  }
  plan
}

arrange_plan_cols <- function(plan) {
  primary <- c("target", "command")
  others <- setdiff(colnames(plan), primary)
  plan[, c(primary, others)]
}

safe_parse <- function(x) {
  out <- parse(text = x, keep.source = FALSE)
  if (length(out)) {
    out <- out[[1]]
  }
  out
}

factor_to_character <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  x
}

# Unnamed arguments may have been declared with `<-`` or `->``
# rather than the required `=`.
warn_arrows <- function(dots) {
  if (!length(dots)) {
    return()
  }
  check_these <- vapply(
    names(dots),
    function(x) !nzchar(x),
    FUN.VALUE = logical(1)
  )
  if (is.null(names(dots))) {
    check_these <- rep(TRUE, length(dots))
  }
  check_these <- which(check_these)
  # Here we use lapply, not vapply, because don't know whether there any
  # offending commands (and thus don't know size of function return)
  offending_commands <- lapply(dots[check_these], detect_arrow)
  offending_commands <- Filter(
    offending_commands,
    f = function(x) {
      !is.null(x)
    }
  )
  if (length(offending_commands)) {
    warning(
      "Use `=` instead of `<-` or `->` ",
      "to assign targets to commands in `drake_plan()`. ",
      "For example, write `drake_plan(a = 1)` instead of ",
      "`drake_plan(a <- 1)`. Arrows were used to declare these commands:\n",
      multiline_message(offending_commands),
      call. = FALSE
    )
  }
}

detect_arrow <- function(command) {
  if (length(command) > 2 && safe_deparse(command[[1]]) %in% c("<-", "->")) {
    safe_deparse(command)
  } else {
    NULL
  }
}

empty_plan <- function() {
  out <- weak_tibble(target = character(0))
  out[["command"]] <- list()
  out
}

tidyeval_exprs <- function(expr_list, envir) {
  lapply(expr_list, tidyeval_expr, envir = envir)
}

tidyeval_expr <- function(expr, envir) {
  call <- as.call(c(quote(rlang::expr), expr))
  eval(call, envir = envir)
}

# weak_as_tibble - use as_tibble() if available but fall back to
# as.data.frame() if necessary
weak_as_tibble <- function(..., .force_df = FALSE) {
  no_tibble <- !suppressWarnings(requireNamespace("tibble", quietly = TRUE))
  if (.force_df || no_tibble) {
    as.data.frame(..., stringsAsFactors = FALSE)
  } else {
    tibble::as_tibble(...)
  }
}

# weak_tibble - use tibble() if available but fall back to
# data.frame() if necessary
weak_tibble <- function(..., .force_df = FALSE) {
  no_tibble <- !suppressWarnings(requireNamespace("tibble", quietly = TRUE))

  if (.force_df || no_tibble) {
    data.frame(..., stringsAsFactors = FALSE)
  } else {
    tibble::tibble(...)
  }
}

complete_target_names <- function(commands_list) {
  if (!length(names(commands_list))) {
    # Should not actually happen, but it's better to have anyway.
    names(commands_list) <- paste0("drake_target_", seq_along(commands_list)) # nocov # nolint
  }
  index <- !nzchar(names(commands_list))
  names(commands_list)[index] <- paste0("drake_target_", seq_len(sum(index)))
  commands_list
}

select_valid_lang <- function(x) {
  discard <- vapply(
    X = x,
    FUN = function(y) {
      identical(y, substitute())
    },
    FUN.VALUE = logical(1)
  )
  x[!discard]
}

as_drake_plan <- function(plan, .force_df = FALSE) {
  no_tibble <- !suppressWarnings(requireNamespace("tibble", quietly = TRUE))
  if (.force_df || no_tibble) {
    structure(
      as.data.frame(plan, stringsAsFactors = FALSE),
      class = c("drake_plan", "data.frame")
    )
  } else {
    tibble::new_tibble(plan, nrow = nrow(plan), subclass = "drake_plan")
  }
}

#' @export
#' @keywords internal
print.drake_plan <- function(x, ...) {
  x <- deparse_lang_cols(x)
  NextMethod(object = x)
}

#' @title Type summary printing
#' @description Ensures `<expr>` is printed at the top
#'   of any `drake` plan column that is a list of language objects
#'   (e.g. `plan$command`).
#' @export
#' @keywords internal
#' @param x List of language objects.
type_sum.expr_list <- function(x) "expr"

#' @export
#' @keywords internal
c.expr_list <- function(x, ...) {
  # Probably won't be covered, but still necessary.
  as_expr_list(NextMethod()) # nocov
}

#' @export
#' @keywords internal
`[.expr_list` <- function(x, i) {
  as_expr_list(NextMethod())
}

as_expr_list <- function(x) {
  structure(x, class = "expr_list")
}

deparse_lang_cols <- function(plan) {
  for (col in lang_cols(plan)) {
    plan[[col]] <- deparse_lang_col(plan[[col]])
  }
  plan
}

deparse_lang_col <- function(x) {
  if (!length(x) || !is.list(x)) {
    return(x)
  }
  out <- unlist(lapply(x, safe_deparse, collapse = " "))
  as_expr_list(out)
}

lang_cols <- function(plan) {
  out <- intersect(colnames(plan), c("command", "trigger", "transform"))
  others <- vapply(
    plan,
    function(x) {
      as.logical(length(x)) && is.list(x) && is.language(x[[1]])
    },
    FUN.VALUE = logical(1)
  )
  union(out, names(which(others)))
}
