#' @title Create a drake plan
#'   for the `plan` argument of [make()].
#' `r lifecycle::badge("stable")`
#'
#' @description A `drake` plan is a data frame with columns
#'   `"target"` and `"command"`. Each target is an R object
#'   produced in your workflow, and each command is the
#'   R code to produce it.
#'
#' @details Besides `"target"` and `"command"`, [drake_plan()]
#'   understands a special set of optional columns. For details, visit
#'   `https://books.ropensci.org/drake/plans.html#special-custom-columns-in-your-plan` # nolint
#'
#' @seealso make, drake_config, transform_plan, map, split, cross, combine
#'
#' @section Columns:
#' [drake_plan()] creates a special data frame. At minimum, that data frame
#' must have columns `target` and `command` with the target names and the
#' R code chunks to build them, respectively.
#'
#' You can add custom columns yourself, either with `target()` (e.g.
#' `drake_plan(y = target(f(x), transform = map(c(1, 2)), format = "fst"))`)
#' or by appending columns post-hoc (e.g. `plan$col <- vals`).
#'
#' Some of these custom columns are special. They are optional,
#' but `drake` looks for them at various points in the workflow.
#' - `transform`: a call to [map()], [split()], [cross()], or
#'   [combine()] to create and manipulate large collections of targets.
#'   Details: (`https://books.ropensci.org/drake/plans.html#large-plans`). # nolint
#' - `format`: set a storage format to save big targets more efficiently.
#'   See the "Formats" section of this help file for more details.
#' - `trigger`: rule to decide whether a target needs to run.
#'   It is recommended that you define this one with `target()`.
#'   Details: `https://books.ropensci.org/drake/triggers.html`.
#' - `hpc`: logical values (`TRUE`/`FALSE`/`NA`) whether to send each target
#'   to parallel workers.
#'   Visit `https://books.ropensci.org/drake/hpc.html#selectivity`
#'   to learn more.
#' - `resources`: target-specific lists of resources for a computing cluster.
#'   See
#'   `https://books.ropensci.org/drake/hpc.html#advanced-options`
#'   for details.
#' - `caching`: overrides the `caching` argument of [make()] for each target
#'   individually. Possible values:
#'   - "main": tell the main process to store the target in the cache.
#'   - "worker": tell the HPC worker to store the target in the cache.
#'   - NA: default to the `caching` argument of [make()].
#' - `elapsed` and `cpu`: number of seconds to wait for the target to build
#'   before timing out (`elapsed` for elapsed time and `cpu` for CPU time).
#' - `retries`: number of times to retry building a target
#'   in the event of an error.
#' - `seed`: an optional pseudo-random number generator (RNG)
#'   seed for each target. `drake` usually comes up with its own
#'   unique reproducible target-specific seeds using the global seed
#'   (the `seed` argument to [make()] and [drake_config()])
#'   and the target names, but you can overwrite these automatic seeds.
#'   `NA` entries default back to `drake`'s automatic seeds.
#' - `max_expand`: for dynamic branching only. Same as the `max_expand`
#'   argument of [make()], but on a target-by-target basis.
#'   Limits the number of sub-targets created for a given target.
#' @section Formats:
#'   Specialized target formats increase efficiency and flexibility.
#'   Some allow you to save specialized objects like `keras` models,
#'   while others increase the speed while conserving storage and memory.
#'   You can declare target-specific formats in the plan
#'   (e.g. `drake_plan(x = target(big_data_frame, format = "fst"))`)
#'   or supply a global default `format` for all targets in `make()`.
#'   Either way, most formats have specialized installation requirements
#'   (e.g. R packages) that are not installed with `drake` by default.
#'   You will need to install them separately yourself.
#'   Available formats:
#'   - `"file"`: Dynamic files. To use this format, simply create
#'     local files and directories yourself and then return
#'     a character vector of paths as the target's value.
#'     Then, `drake` will watch for changes to those files in
#'     subsequent calls to `make()`. This is a more flexible
#'     alternative to `file_in()` and `file_out()`, and it is
#'     compatible with dynamic branching.
#'     See `https://github.com/ropensci/drake/pull/1178` for an example.
#'   - `"fst"`: save big data frames fast. Requires the `fst` package.
#'     Note: this format strips non-data-frame attributes such as the
#'   - `"fst_tbl"`: Like `"fst"`, but for `tibble` objects.
#'     Requires the `fst` and `tibble` packages.
#'     Strips away non-data-frame non-tibble attributes.
#'   - `"fst_dt"`: Like `"fst"` format, but for `data.table` objects.
#'     Requires the `fst` and `data.table` packages.
#'     Strips away non-data-frame non-data-table attributes.
#'   - `"diskframe"`:
#'     Stores `disk.frame` objects, which could potentially be
#'     larger than memory. Requires the `fst` and `disk.frame` packages.
#'     Coerces objects to `disk.frame`s.
#'     Note: `disk.frame` objects get moved to the `drake` cache
#'     (a subfolder of `.drake/` for most workflows).
#'     To ensure this data transfer is fast, it is best to
#'     save your `disk.frame` objects to the same physical storage
#'     drive as the `drake` cache,
#'     `as.disk.frame(your_dataset, outdir = drake_tempfile())`.
#'   - `"keras"`: save Keras models as HDF5 files.
#'     Requires the `keras` package.
#'   - `"qs"`: save any R object that can be properly serialized
#'     with the `qs` package. Requires the `qs` package.
#'     Uses `qsave()` and `qread()`.
#'     Uses the default settings in `qs` version 0.20.2.
#'   - `"rds"`: save any R object that can be properly serialized.
#'     Requires R version >= 3.5.0 due to ALTREP.
#'     Note: the `"rds"` format uses gzip compression, which is slow.
#'     `"qs"` is a superior format.
#'
#' @section Keywords:
#' [drake_plan()] understands special keyword functions for your commands.
#' With the exception of [target()], each one is a proper function
#' with its own help file.
#' - [target()]: give the target more than just a command.
#'   Using [target()], you can apply a transformation
#'   (examples: `https://books.ropensci.org/drake/plans.html#large-plans`), # nolint
#'   supply a trigger (`https://books.ropensci.org/drake/triggers.html`), # nolint
#'   or set any number of custom columns.
#' - [file_in()]: declare an input file dependency.
#' - [file_out()]: declare an output file to be produced
#'   when the target is built.
#' - [knitr_in()]: declare a `knitr` file dependency such as an
#'   R Markdown (`*.Rmd`) or R LaTeX (`*.Rnw`) file.
#' - [ignore()]: force `drake` to entirely ignore a piece of code:
#'   do not track it for changes and do not analyze it for dependencies.
#' - [no_deps()]: tell `drake` to not track the dependencies
#'   of a piece of code. `drake` still tracks the code itself for changes.
#' - [id_chr()]: Get the name of the current target.
#' - [drake_envir()]: get the environment where drake builds targets.
#'   Intended for advanced custom memory management.
#'
#' @inheritSection transformations Transformations
#' @inheritSection transformations Static branching
#' @inheritSection transformations Dynamic branching
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
#' \dontrun{
#' isolate_example("contain side effects", {
#' # For more examples, visit
#' # https://books.ropensci.org/drake/plans.html.
#'
#' # Create drake plans:
#' mtcars_plan <- drake_plan(
#'   write.csv(mtcars[, c("mpg", "cyl")], file_out("mtcars.csv")),
#'   value = read.csv(file_in("mtcars.csv"))
#' )
#' if (requireNamespace("visNetwork", quietly = TRUE)) {
#'   plot(mtcars_plan) # fast simplified call to vis_drake_graph()
#' }
#' mtcars_plan
#' make(mtcars_plan) # Makes `mtcars.csv` and then `value`
#' head(readd(value))
#' # You can use knitr inputs too. See the top command below.
#'
#' load_mtcars_example()
#' head(my_plan)
#' if (requireNamespace("knitr", quietly = TRUE)) {
#'   plot(my_plan)
#' }
#' # The `knitr_in("report.Rmd")` tells `drake` to dive into the active
#' # code chunks to find dependencies.
#' # There, `drake` sees that `small`, `large`, and `coef_regression2_small`
#' # are loaded in with calls to `loadd()` and `readd()`.
#' deps_code("report.Rmd")
#'
#' # Formats are great for big data: https://github.com/ropensci/drake/pull/977
#' # Below, each target is 1.6 GB in memory.
#' # Run make() on this plan to see how much faster fst is!
#' n <- 1e8
#' plan <- drake_plan(
#'   data_fst = target(
#'     data.frame(x = runif(n), y = runif(n)),
#'     format = "fst"
#'   ),
#'   data_old = data.frame(x = runif(n), y = runif(n))
#' )
#'
#' # Use transformations to generate large plans.
#' # Read more at
#' # `https://books.ropensci.org/drake/plans.html#create-large-plans-the-easy-way`. # nolint
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
#'     analyze(large_data),
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
#'
#' # Dynamic branching
#' # Get the mean mpg for each cyl in the mtcars dataset.
#' plan <- drake_plan(
#'   raw = mtcars,
#'   group_index = raw$cyl,
#'   munged = target(raw[, c("mpg", "cyl")], dynamic = map(raw)),
#'   mean_mpg_by_cyl = target(
#'     data.frame(mpg = mean(munged$mpg), cyl = munged$cyl[1]),
#'     dynamic = group(munged, .by = group_index)
#'   )
#' )
#' make(plan)
#' readd(mean_mpg_by_cyl)
#' })
#' }
drake_plan <- function(
  ...,
  list = NULL,
  file_targets = NULL,
  strings_in_dots = NULL,
  tidy_evaluation = NULL,
  transform = TRUE,
  trace = FALSE,
  envir = parent.frame(),
  tidy_eval = TRUE,
  max_expand = NULL
) {
  deprecate_arg(file_targets, "file_targets") # 2019-02-01 nolint
  deprecate_arg(strings_in_dots, "strings_in_dots") # 2019-02-01 nolint
  deprecate_arg(list, "list") # 2019-02-01 nolint
  deprecate_arg(tidy_evaluation, "tidy_evaluation", "tidy_eval") # 2019-04-02 # nolint
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
  commands <- unname(commands)
  plan <- weak_tibble(target = targets)
  plan$command <- commands
  plan <- parse_custom_plan_columns(plan, envir = envir)
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
    plan <- tidyeval_cols(plan, envir = envir)
  }
  sanitize_plan(plan, envir = envir)
}

parse_custom_plan_columns <- function(plan, envir) {
  Sys.setenv("drake_target_silent" = "true")
  on.exit(Sys.setenv("drake_target_silent" = ""))
  splits <- base::split(plan, seq_len(nrow(plan)))
  out <- lapply(splits, parse_custom_plan_row, envir = envir)
  out <- do.call(drake_bind_rows, out)
}

parse_custom_plan_row <- function(row, envir) {
  expr <- row$command
  if (!length(expr) || !is_target_call(expr[[1]])) {
    return(row)
  }
  expr[[1]][[1]] <- namespaced_target
  out <- eval(expr[[1]], envir = envir)
  out$target <- row$target
  out
}

namespaced_target <- parse(text = ("drake:::target"))[[1]]

is_target_call <- function(expr) {
  tryCatch(
    safe_deparse(expr[[1]], backtick = FALSE) %in% target_fns,
    error = error_false
  )
}

drake_bind_rows <- function(...) {
  args <- rlang::dots_list(..., .ignore_empty = "all")
  df_env <- new.env(parent = emptyenv())
  df_env$dfs <- list()
  flatten_df_list(args, df_env = df_env)
  dfs <- df_env$dfs
  cols <- lapply(dfs, colnames)
  cols <- Reduce(f = base::union, x = cols)
  dfs <- lapply(dfs, fill_cols, cols = cols)
  do.call(rbind, dfs)
}

flatten_df_list <- function(args, df_env) {
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

sanitize_plan <- function(
  plan,
  allow_duplicated_targets = FALSE,
  envir = parent.frame()
) {
  is_plan <- is.data.frame(plan) &&
    all(c("target", "command") %in% colnames(plan))
  if (!is_plan) {
    stop0(
      "the drake plan must be a data frame that contains ",
      "columns `target` and `command`."
    )
  }
  if (nrow(plan) < 1L) {
    return(plan)
  }
  force(envir)
  fields <- intersect(colnames(plan), c("command", "target", "trigger"))
  for (field in fields) {
    plan[[field]] <- factor_to_character(plan[[field]])
    if (is.character(plan[[field]])) {
      plan[[field]] <- trimws(plan[[field]])
    }
  }
  plan$target <- make.names(plan$target, unique = FALSE, allow_ = TRUE)
  plan$target <- convert_trailing_dot(plan$target)
  plan <- plan[nzchar(plan$target), ]
  first <- c("target", "command")
  cols <- c(first, setdiff(colnames(plan), first))
  if (!allow_duplicated_targets) {
    plan <- assert_unique_targets(plan[, cols])
  }
  plan <- arrange_plan_cols(plan)
  plan <- eval_non_lang_cols(plan, envir = envir)
  plan <- parse_lang_cols(plan)
  plan$command <- lapply(plan$command, sanitize_command)
  as_drake_plan(plan)
}

sanitize_command <- function(command) {
  if (!is.language(command)) {
    command <- safe_parse(deparse(command))
  }
  if (is.null(command)) {
    command <- quote(c())
  }
  command
}

# https://github.com/ropensci/drake/issues/1147
convert_trailing_dot <- function(x) {
  index <- grepl("\\.$", x)
  if (any(index)) {
    warn0("removed trailing dot from some target names.")
  }
  x[index] <- gsub("\\.$", "_", x[index])
  x
}

assert_unique_targets <- function(plan) {
  dups <- duplicated(plan$target)
  if (any(dups)) {
    stop0(
      "duplicated target names:\n",
      multiline_message(plan$target[dups])
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
    warn0(
      "Use `=` instead of `<-` or `->` ",
      "to assign targets to commands in `drake_plan()`. ",
      "For example, write `drake_plan(a = 1)` instead of ",
      "`drake_plan(a <- 1)`. Arrows were used to declare these commands:\n",
      multiline_message(offending_commands)
    )
  }
}

detect_arrow <- function(command) {
  has_arrow <- length(command) > 2 &&
    safe_deparse(command[[1]], backtick = FALSE) %in% c("<-", "->")
  if (has_arrow) {
    safe_deparse(command, backtick = TRUE)
  } else {
    NULL
  }
}

empty_plan <- function() {
  out <- weak_tibble(target = character(0))
  out[["command"]] <- list()
  out
}

tidyeval_cols <- function(plan, envir) {
  for (col in setdiff(colnames(plan), c("target", "transform"))) {
    plan[[col]] <- tidyeval_exprs(plan[[col]], envir = envir)
  }
  plan
}

tidyeval_exprs <- function(expr_list, envir) {
  lapply(expr_list, tidyeval_expr, envir = envir)
}

tidyeval_expr <- function(expr, envir) {
  call <- as.call(c(quote(rlang::expr), expr))
  eval(call, envir = envir)
}

eval_non_lang_cols <- function(plan, envir) {
  for (col in non_lang_cols(plan)) {
    plan[[col]] <- eval_non_lang_col(plan[[col]], envir = envir)
  }
  plan
}

eval_non_lang_col <- function(x, envir) {
  if (any(vlapply(x, is.language))) {
    x <- lapply(x, eval, envir = envir)
  }
  if (all(vlapply(x, is.atomic))) {
    x <- unlist(x)
  }
  x
}

parse_lang_cols <- function(plan) {
  for (col in lang_cols(plan)) {
    if (!is.list(plan[[col]])) {
      plan[[col]] <- lapply(plan[[col]], safe_parse)
    }
    plan[[col]] <- lapply(plan[[col]], replace_missing_symbol)
  }
  plan
}

replace_missing_symbol <- function(x) {
  if (identical(x, substitute())) {
    x <- expression(NULL)
  }
  x
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
      class = c("drake_plan", "drake", "data.frame")
    )
  } else {
    tibble::new_tibble(plan, nrow = nrow(plan), class = "drake_plan")
  }
}

#' @export
#' @keywords internal
plot.drake_plan <- function(x, ...) {
  config <- drake_config(
    x,
    envir = new.env(parent = baseenv()),
    verbose = 0L,
    cache = storr::storr_environment(),
    history = FALSE,
    recoverable = FALSE,
    session_info = FALSE
  )
  vis_drake_graph_impl(
    config,
    build_times = "none",
    targets_only = TRUE,
    main = "",
    hover = FALSE,
    make_imports = FALSE,
    from_scratch = TRUE
  )
}

#' @export
#' @keywords internal
print.drake_plan <- function(x, ...) {
  x <- deparse_lang_cols(x)
  NextMethod(object = x)
}

# for installation on R 3.3.0
type_sum <- NULL

#' @title Type summary printing
#' `r lifecycle::badge("questioning")`
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
  out <- unlist(lapply(x, safe_deparse, collapse = " ", backtick = TRUE))
  as_expr_list(out)
}

lang_cols <- function(plan) {
  intersect(colnames(plan), c("command", "dynamic", "trigger", "transform"))
}

non_lang_cols <- function(plan) {
  setdiff(colnames(plan), c("command", "dynamic", "trigger", "transform"))
}
