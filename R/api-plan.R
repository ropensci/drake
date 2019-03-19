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
#'  where `f()` is either `map()`, `cross()`, or `combine()`
#'  (similar to `purrr::pmap()`, `tidy::crossing()`, and `dplyr::summarize()`,
#'  respectively). These verbs mimic Tidyverse behavior to scale up
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
#' @param ... A collection of symbols/targets
#'   with commands assigned to them. See the examples for details.
#' @param list Deprecated
#' @param file_targets Deprecated.
#' @param strings_in_dots Deprecated.
#' @param tidy_evaluation Logical, whether to use tidy evaluation
#'   such as quasiquotation
#'   when evaluating commands passed through the free-form
#'   `...` argument.
#' @param transform Logical, whether to transform the plan
#'   into a larger plan with more targets.
#'   This is still an experimental feature,
#'   so please check your workflow with `vis_drake_graph()`
#'   before running it with `make()`.
#'   Requires the `transform` and `group` fields identified
#'   by `target()`. See the examples for details.
#' @param trace Logical, whether to add columns to show
#'   what happens during target transformations.
#' @param envir Environment for tidy evaluation.
#' @examples
#' test_with_dir("Contain side effects", {
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
#' # This feature is experimental, so please
#' # check your workflow with `vis_drake_graph()`
#' # before running `make()`.
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
  envir = parent.frame()
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
    plan <- transform_plan(plan, envir = envir, trace = trace)
  }
  if (tidy_evaluation %||% TRUE) {
    for (col in setdiff(colnames(plan), c("target", "transform"))) {
      plan[[col]] <- tidyeval_exprs(plan[[col]], envir = envir)
    }
  }
  for (col in setdiff(colnames(plan), c("target", "command", "trigger"))) {
    plan[[col]] <- unlist(plan[[col]])
  }
  plan <- resolve_drake_pipe(plan, envir)
  sanitize_plan(plan)
}

#' @title Row-bind together drake plans
#' @description Combine drake plans together in a way that
#'   correctly fills in missing entries.
#' @export
#' @seealso [drake_plan()], [make()]
#' @param ... Workflow plan data frames (see [drake_plan()]).
#' @examples
#' # You might need to refresh your data regularly (see ?triggers).
#' download_plan <- drake_plan(
#'   data = target(
#'     command = download_data(),
#'     trigger = "always"
#'   )
#' )
#' # But if the data don't change, the analyses don't need to change.
#' analysis_plan <- drake_plan(
#'   usage = get_usage_metrics(data),
#'   topline = scrape_topline_table(data)
#' )
#' your_plan <- bind_plans(download_plan, analysis_plan)
#' your_plan
#' # make(your_plan) # nolint
bind_plans <- function(...) {
  sanitize_plan(drake_bind_rows(...))
}

parse_custom_plan_columns <- function(plan) {
  Sys.setenv("drake_target_silent" = "true")
  on.exit(Sys.setenv("drake_target_silent" = ""))
  splits <- split(plan, seq_len(nrow(plan)))
  out <- lapply(splits, parse_custom_plan_row)
  do.call(bind_plans, out)
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

complete_target_names <- function(commands_list) {
  if (!length(names(commands_list))) {
    # Should not actually happen, but it's better to have anyway.
    names(commands_list) <- paste0("drake_target_", seq_along(commands_list)) # nocov # nolint
  }
  index <- !nzchar(names(commands_list))
  names(commands_list)[index] <- paste0("drake_target_", seq_len(sum(index)))
  commands_list
}

#' @title Declare the file inputs of a workflow plan command.
#' @description Use this function to help write the commands
#'   in your workflow plan data frame. See the examples
#'   for a full explanation.
#' @export
#' @seealso [file_out()], [knitr_in()], [ignore()]
#' @return A character vector of declared input file paths.
#' @param ... Character strings. File paths of input files
#'   to a command in your workflow plan data frame.
#' @export
#' @examples
#' \dontrun{
#' test_with_dir("Contain side effects", {
#' # The `file_out()` and `file_in()` functions
#' # just takes in strings and returns them.
#' file_out("summaries.txt")
#' # Their main purpose is to orchestrate your custom files
#' # in your workflow plan data frame.
#' suppressWarnings(
#'   plan <- drake_plan(
#'     write.csv(mtcars, file_out("mtcars.csv")),
#'     contents = read.csv(file_in("mtcars.csv"))
#'   )
#' )
#' plan
#' # drake knows "\"mtcars.csv\"" is the first target
#' # and a dependency of `contents`. See for yourself:
#' make(plan)
#' file.exists("mtcars.csv")
#' # See also `knitr_in()`. `knitr_in()` is like `file_in()`
#' # except that it analyzes active code chunks in your `knitr`
#' # source file and detects non-file dependencies.
#' # That way, updates to the right dependencies trigger rebuilds
#' # in your report.
#' })
#' }
file_in <- function(...) {
  as.character(c(...))
}

#' @title Declare the file outputs of a workflow plan command.
#' @description Use this function to help write the commands
#'   in your workflow plan data frame. You can only specify
#'   one file output per command. See the examples
#'   for a full explanation.
#' @export
#' @seealso [file_in()], [knitr_in()], [ignore()]
#' @return A character vector of declared output file paths.
#' @param ... Character vector of output file paths.
#' @examples
#' \dontrun{
#' test_with_dir("Contain side effects", {
#' # The `file_out()` and `file_in()` functions
#' # just takes in strings and returns them.
#' file_out("summaries.txt", "output.csv")
#' # Their main purpose is to orchestrate your custom files
#' # in your workflow plan data frame.
#' suppressWarnings(
#'   plan <- drake_plan(
#'     write.csv(mtcars, file_out("mtcars.csv")),
#'     contents = read.csv(file_in("mtcars.csv"))
#'   )
#' )
#' plan
#' # drake knows "\"mtcars.csv\"" is the first target
#' # and a dependency of `contents`. See for yourself:
#' make(plan)
#' file.exists("mtcars.csv")
#' # See also `knitr_in()`. `knitr_in()` is like `file_in()`
#' # except that it analyzes active code chunks in your `knitr`
#' # source file and detects non-file dependencies.
#' # That way, updates to the right dependencies trigger rebuilds
#' # in your report.
#' })
#' }
file_out <- file_in

#' @title Declare the `knitr`/`rmarkdown` source files
#'   of a workflow plan command.
#' @description Use this function to help write the commands
#'   in your workflow plan data frame. See the examples
#'   for a full explanation.
#' @export
#' @seealso [file_in()], [file_out()], [ignore()]
#' @return A character vector of declared input file paths.
#' @param ... Character strings. File paths of `knitr`/`rmarkdown`
#'   source files supplied to a command in your workflow plan data frame.
#' @examples
#' \dontrun{
#' test_with_dir("Contain side effects", {
#' if (suppressWarnings(require("knitr"))) {
#' # `knitr_in()` is like `file_in()`
#' # except that it analyzes active code chunks in your `knitr`
#' # source file and detects non-file dependencies.
#' # That way, updates to the right dependencies trigger rebuilds
#' # in your report.
#' # The mtcars example (`drake_example("mtcars")`)
#' # already has a demonstration
#' load_mtcars_example()
#' make(my_plan)
#' # Now how did drake magically know that
#' # `small`, `large`, and `coef_regression2_small` were
#' # dependencies of the output file `report.md`?
#' # because the command in the workflow plan had
#' # `knitr_in("report.Rmd")` in it, so drake knew
#' # to analyze the active code chunks. There, it spotted
#' # where `small`, `large`, and `coef_regression2_small`
#' # were read from the cache using calls to `loadd()` and `readd()`.
#' }
#' })
#' }
knitr_in <- file_in

#' @title Ignore components of commands and imported functions.
#' @description In a command in the workflow plan
#' or the body of an imported function, you can
#' `ignore(some_code)` to
#' 1. Force `drake` to not track dependencies in `some_code`, and
#' 2. Ignore any changes in `some_code` when it comes to
#'   deciding which target are out of date.
#' @export
#' @seealso [file_in()], [file_out()], [knitr_in()]
#' @return The argument.
#' @param x Code to ignore.
#' @examples
#' \dontrun{
#' test_with_dir("Contain side effects", {
#' # Normally, `drake` reacts to changes in dependencies.
#' x <- 4
#' make(plan = drake_plan(y = sqrt(x)))
#' x <- 5
#' make(plan = drake_plan(y = sqrt(x)))
#' make(plan = drake_plan(y = sqrt(4) + x))
#' # But not with ignore().
#' make(plan = drake_plan(y = sqrt(4) + ignore(x))) # Builds y.
#' x <- 6
#' make(plan = drake_plan(y = sqrt(4) + ignore(x))) # Skips y.
#' make(plan = drake_plan(y = sqrt(4) + ignore(x + 1))) # Skips y.
#' # What about imported functions?
#' f <- function(x) sqrt(4) + ignore(x + 1)
#' make(plan = drake_plan(x = f(2)))
#' readd(x)
#' f <- function(x) sqrt(4) + ignore(x + 2)
#' make(plan = drake_plan(x = f(2)))
#' readd(x)
#' f <- function(x) sqrt(5) + ignore(x + 2)
#' make(plan = drake_plan(x = f(2)))
#' readd(x)
#' })
#' }
ignore <- function(x = NULL) {
  identity(x)
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

#' @title Get the environment where drake builds targets
#' @description Call this function inside the commands in your plan
#'   to get the environment where `drake` builds targets.
#'   That way, you can strategically remove targets from memory
#'   while [make()] is running. That way, you can limit the
#'   amount of computer memory you use.
#' @export
#' @seealso [from_plan()]
#' @return The environment where `drake` builds targets.
#' @examples
#' plan <- drake_plan(
#'   large_data_1 = sample.int(1e4),
#'   large_data_2 = sample.int(1e4),
#'   subset = c(large_data_1[seq_len(10)], large_data_2[seq_len(10)]),
#'   summary = {
#'     print(ls(envir = drake_envir()))
#'     # We don't need the large_data_* targets in memory anymore.
#'     rm(large_data_1, large_data_2, envir = drake_envir())
#'     print(ls(envir = drake_envir()))
#'     mean(subset)
#'   }
#' )
#' make(plan, cache = storr::storr_environment(), session_info = FALSE)
drake_envir <- function() {
  envir <- environment()
  for (i in seq_len(getOption("expressions"))) {
    if (exists(drake_plan_marker, envir = envir, inherits = FALSE)) {
      return(envir)
    }
    if (identical(envir, globalenv())) {
      break # nocov
    }
    envir <- parent.frame(n = i)
  }
  stop(
    "Could not find the environment where drake builds targets. ",
    "drake_envir() should only be called inside commands ",
    "in your workflow plan data frame.",
    call. = FALSE
  )
}

#' @title Get a target's plan info from inside a plan's command.
#' @description Call this function inside the commands in your plan
#'   to get an entry from any column in the plan. Changes to custom
#'   columns referred to this way
#'   (for example, `a` in `drake_plan(x = target(from_plan("a"), a = 123))`)
#'   do not invalidate targets, so be careful. Only use `from_plan()`
#'   to reference data that does not actually affect the output value
#'   of the target. For example, you might use `from_plan()` to set the
#'   number of parallel workers within a target:
#'   `drake_plan(x = target(mclapply(..., from_plan("cores"), cores = 4))`.
#'   Now, if you change the `cores` column of the plan, the parallelism will
#'   change, but the target `x` will stay up to date.
#' @export
#' @seealso [drake_envir()]
#' @return `plan[target, column]`, where `plan` is your workflow
#'   plan data frame, `target` is the target being built,
#'   and `column` is the name of the column of the plan you provide.
#' @param column Character, name of a column in your `drake` plan.
#' @examples
#' plan <- drake_plan(my_target = target(from_plan("a"), a = "a_value"))
#' plan
#'
#' cache <- storr::storr_environment()
#' make(plan, cache = cache, session_info = FALSE)
#' readd(my_target, cache = cache)
#'
#' # Why do we care?
#' # Because this is a good way to apply parallel computing within targets
#' # and keep them up to date even when we change
#' # the number of "cores"
#'
#' plan <- drake_plan(
#'   a = target(
#'     parallel::mclapply(1:8, sqrt, mc.cores = from_plan("cores")),
#'     cores = 4
#'   ),
#'   b = target(
#'     parallel::mclapply(1:4, sqrt, mc.cores = from_plan("cores")),
#'     cores = 2
#'   )
#' )
#'
#' plan
#'
#' # If make(plan, parallelism = "loop") fails,
#' # try make(plan, lock_envir = FALSE)
#' # or another parallel computing option like parLapply()
#' # or furrr::future_map().
#' # See https://github.com/ropensci/drake/issues/675#issuecomment-454403818
#' # and the ensuing comments for a discussion.
#'
#' # But usually, parallelism *among* targets happens through a cluster.
#' # drake_hpc_template_file("slurm_clustermq.tmpl") # Edit by hand. # nolint
#' # options(
#' #   clustermq.scheduler = "slurm",
#' #   clustermq.template = "slurm_clustermq.tmpl",
#' # )
#' # make(plan, parallelism = "clustermq", jobs = 2) # nolint
#'
#' # Now, if you change the `cores` column of the plan, the parallelism will
#' # change, but the targets will stay up to date.
#' plan$cores <- c(1, 1)
#' plan
#'
#' # make(plan) # nolint
from_plan <- function(column) {
  envir <- drake_envir()
  plan <- get(drake_plan_marker, envir = envir)
  target <- get(drake_target_marker, envir = envir)
  plan[[column]][plan$target == target]
}

drake_plan_marker <- "._drake_plan"
drake_target_marker <- "._drake_target"

advertise_dsl <- function() {
  on.exit(Sys.setenv(drake_dsl_advertised = "true"))
  if (nchar(Sys.getenv("drake_dsl_advertised"))) {
    return(invisible())
  }
  message(
    "The interface at ",
    "https://ropenscilabs.github.io/drake-manual/plans.html#large-plans ",
    "is better than evaluate_plan(), map_plan(), gather_by(), etc."
  )
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

empty_plan <- function() {
  out <- weak_tibble(target = character(0))
  out[["command"]] <- list()
  out
}

#' @export
#' @keywords internal
print.drake_plan <- function(x, ...) {
  x <- deparse_lang_cols(x)
  NextMethod(object = x)
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

#' @title Type summary printing
#' @description Ensures `<expr>` is printed at the top
#'   of any `drake` plan column that is a list of language objects
#'   (e.g. `plan$command`).
#' @export
#' @keywords internal
#' @param x List of language objects.
type_sum.expr_list <- function(x) "expr"

as_expr_list <- function(x) {
  structure(x, class = "expr_list")
}

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
