#' @title Create a workflow plan data frame
#'   for the `plan` argument of [make()].
#' @description Turns a named collection of target/command pairs into
#' a workflow plan data frame for [make()]. You can give the commands
#' as named expressions, or you can use the `list`
#' argument to supply them as character strings.
#' @details A workflow plan data frame is a data frame
#' with a `target` column and a `command` column.
#' Targets are the R objects that `drake` generates,
#' and commands are the pieces of R code that produce them.
#'
#' The commands that return targets may also depend on
#' external files and create multiple external files.
#' To signal that you are creating and/or depending on
#' custom files in your commands,
#' use the [file_in()], [knitr_in()], and
#' [file_out()] functions in your commands.
#' the examples in this help file provide some guidance.
#'
#' Besides the `target` and `command` columns, there are optional columns
#' you may append to your workflow plan data frame:
#' - `trigger`: a character vector of triggers. A trigger is a rule for
#' when to cause a target to (re)build. See [triggers()] for your options.
#' For a walkthrough, see
#' <https://ropenscilabs.github.io/drake-manual/debug.html>
#' - `retries`: number of times to retry a target if it fails
#'   to build the first time.
#' - `timeout`: Seconds of overall time to allow before imposing
#'   a timeout on a target.
#'   Assign target-level timeout times with an optional `timeout`
#'   column in `plan`.
#' - `cpu`: Seconds of cpu time to allow before imposing
#'   a timeout on a target.
#'   Assign target-level cpu timeout times with an optional `cpu`
#'   column in `plan`.
#' - `elapsed`: Seconds of elapsed time to allow before imposing
#'   a timeout on a target.
#'   Assign target-level elapsed timeout times with an optional `elapsed`
#'   column in `plan`.
#' - `resources`: Experimental, no guarantees that this works all the time.
#'   `resources` is a list column. Each element is a named list,
#'   same as the `resources` argument to `batchtools_slurm()`
#'   and related `future.bachtools` functions. See also
#'   <https://github.com/HenrikBengtsson/future.batchtools#examples>. # nolint
#'   `resources[[target_name]]` is a list of
#'   computing resource parameters for the target.
#'   Each element is a value passed to a `brew` placeholder of a
#'   `batchtools` template file.
#'   The list names of `resources[[target_name]]`
#'   should be the brew patterns.
#'
#' @export
#' @seealso [map_plan()], [reduce_by()], [gather_by()], [reduce_plan()], [gather_plan()],
#'   [evaluate_plan()], [expand_plan()]
#' @return A data frame of targets and commands. See the details
#' for optional columns you can append manually post-hoc.
#' @param ... A collection of symbols/targets
#'   with commands assigned to them. See the examples for details.
#' @param list A named character vector of commands
#'   with names as targets.
#' @param file_targets Deprecated.
#' @param strings_in_dots Deprecated.
#' @param tidy_evaluation Logical, whether to use tidy evaluation
#'   such as quasiquotation
#'   when evaluating commands passed through the free-form
#'   `...` argument.
#' @param transform Logical, whether to transform targets in the plan
#'   according to the `transform` and `group` fields identified
#'   by `target()`. See the examples for details.
#' @param trace Logical, whether to add columns to show
#'   what happened during target transformations, e.g.
#'   `drake_plan(x = target(..., transform = ...), transform = TRUE)`.
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
#' drake_plan(
#'   small = simulate(48),
#'   large = simulate(64),
#'   reg = target(
#'     reg_fun(data),
#'    transform = cross(reg_fun = c(reg1, reg2), data = c(small, large))
#'   ),
#'   summ = target(
#'     sum_fun(data, reg),
#'    transform = cross(sum_fun = c(coef, residuals), reg)
#'   ), 
#'   winners = target(
#'     min(summ),
#'     transform = summarize(data, sum_fun)
#'   )
#' )
#'
#' # Define custom groupings with the `group` field.
#' drake_plan(
#'   small = simulate(48),
#'   large = simulate(64),
#'   reg1 = target(
#'     reg_fun(data),
#'     transform = cross(data = c(small, large)),
#'     group = reg
#'   ),
#'   reg2 = target(
#'     reg_fun(data),
#'     transform = cross(data = c(small, large)),
#'     group = reg
#'   ),
#'   winners = target(
#'     min(reg),
#'     transform = summarize(data),
#'     a = 1
#'   )
#' )
#'
#' # Set `trace` to `TRUE` to retain information about the
#' # transformation process.
#' drake_plan(
#'   small = simulate(48),
#'   large = simulate(64),
#'   reg = target(
#'     reg_fun(data),
#'    transform = cross(reg_fun = c(reg1, reg2), data = c(small, large))
#'   ),
#'   summ = target(
#'     sum_fun(data, reg),
#'    transform = cross(sum_fun = c(coef, residuals), reg)
#'   ), 
#'   winners = target(
#'     min(summ),
#'     transform = summarize(data, sum_fun)
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
#' # Are you a fan of tidy evaluation?
#' my_variable <- 1
#' drake_plan(
#'   a = !!my_variable,
#'   b = !!my_variable + 1,
#'   list = c(d = "!!my_variable")
#' )
#' drake_plan(
#'   a = !!my_variable,
#'   b = !!my_variable + 1,
#'   list = c(d = "!!my_variable"),
#'   tidy_evaluation = FALSE
#' )
#' # For instances of !! that remain unevaluated in the workflow plan,
#' # make() will run these commands in tidy fashion,
#' # evaluating the !! operator using the environment you provided.
#' })
drake_plan <- function(
  ...,
  list = character(0),
  file_targets = NULL,
  strings_in_dots = NULL,
  tidy_evaluation = TRUE,
  transform = TRUE,
  trace = FALSE
) {
  if (tidy_evaluation) {
    dots <- rlang::exprs(...) # Enables quasiquotation via rlang.
  } else {
    dots <- match.call(expand.dots = FALSE)$...
  }
  warn_arrows(dots)
  commands_dots <- lapply(dots, wide_deparse)
  names(commands_dots) <- names(dots)
  commands <- c(commands_dots, list)
  if (!length(commands)) {
    return(
      weak_tibble(
        target = character(0),
        command = character(0)
      )
    )
  }
  commands <- complete_target_names(commands)
  targets <- names(commands)
  commands <- as.character(commands)
  plan <- weak_tibble(
    target = targets,
    command = commands
  )
  from_dots <- plan$target %in% names(commands_dots)
  if (length(file_targets) || length(strings_in_dots)) {
    warning(
      "Arguments `file_targets` and `strings_in_dots` ",
      "of `drake_plan()` are deprecated.",
      call. = FALSE
    )
  }
  plan <- parse_custom_plan_columns(plan)
  if (transform && ("transform" %in% colnames(plan))) {
    plan <- trf_plan(plan, trace = trace)
  }
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
  args <- list(...)
  plan_env <- new.env(parent = emptyenv())
  plan_env$plans <- list()
  flatten_plan_list(args, plan_env = plan_env)
  plans <- plan_env$plans
  plans <- Filter(f = nrow, x = plans)
  plans <- Filter(f = ncol, x = plans)
  cols <- lapply(plans, colnames)
  cols <- Reduce(f = union, x = cols)
  plans <- lapply(plans, fill_cols, cols = cols)
  plan <- do.call(rbind, plans)
  sanitize_plan(plan)
}

flatten_plan_list <- function(args, plan_env){
  if (!is.null(dim(args))) {
    index <- length(plan_env$plans) + 1
    plan_env$plans[[index]] <- weak_as_tibble(args)
  } else {
    lapply(args, flatten_plan_list, plan_env = plan_env)
  }
}

fill_cols <- function(x, cols) {
  na_cols <- setdiff(cols, colnames(x))
  x[, na_cols] <- NA
  x
}

handle_duplicated_targets <- function(plan) {
  plan <- plan[!duplicated(plan[, c("target", "command")]), ]
  dups <- duplicated(plan$target)
  if (any(dups)) {
    stop(
      "Duplicated targets with different commands:\n",
      multiline_message(plan$target[dups]),
      call. = FALSE
    )
  }
  plan
}

parse_custom_plan_columns <- function(plan) {
  splits <- split(plan, seq_len(nrow(plan)))
  out <- lapply(splits, parse_custom_plan_row)
  do.call(bind_plans, out)
}

parse_custom_plan_row <- function(row) {
  expr <- parse(text = row$command, keep.source = FALSE)
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
  if (is.null(names(dots))) {
    # Probably not possible, but good to have:
    names(dots) <- rep("", length(dots)) # nocov
  }
  check_these <- vapply(names(dots),
                        function(x) !nzchar(x),
                        FUN.VALUE = logical(1))
  check_these <- which(check_these)
  # Here we use lapply, not vapply, because don't know whether there any
  # offending commands (and thus don't know size of function return)
  offending_commands <- lapply(dots[check_these], detect_arrow)
  offending_commands <- Filter(
    offending_commands,
    f = function(x) {
      !is.null(x)
    })
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
  if (length(command) > 2 && deparse(command[[1]]) %in% c("<-", "->")) {
    wide_deparse(command)
  } else {
    NULL
  }
}

#' @title Define custom columns in a [drake_plan()].
#' @description The `target()` function lets you define
#'   custom columns in a workflow plan data frame, both
#'   inside and outside calls to [drake_plan()].
#' @details Tidy evaluation is applied to the arguments,
#'    and the `!!` operator is evaluated immediately
#'    for expressions and language objects.
#' @export
#' @seealso [drake_plan()], [make()]
#' @return A one-row workflow plan data frame with the named
#' arguments as columns.
#' @param command The command to build the target.
#' @param trigger The target's trigger.
#' @param retries Number of retries in case of failure.
#' @param timeout Overall timeout (in seconds) for building a target.
#' @param cpu cpu Timeout (seconds) for building a target.
#' @param elapsed Elapsed time (seconds) for building a target.
#' @param priority Integer giving the build priority of a target.
#'   Given two targets about to be built at the same time,
#'   the one with the lesser priority (numerically speaking)
#'   will be built first.
#' @param worker The preferred worker to be assigned the target
#'   (in parallel computing).
#' @param resources Experimental, no guarantees that this works all the time.
#'   Same as the `resources` argument to `batchtools_slurm()`
#'   and related `future.bachtools` functions.
#'   See also <https://github.com/HenrikBengtsson/future.batchtools#examples>. # nolint
#'   `resources` is a list of computing resource parameters for the target.
#'   Each element is a value passed to a `brew` placeholder of a
#'   `batchtools` template file. The list names of `resources`
#'   should be the brew patterns.
#' @param ... Named arguments specifying non-standard
#'   fields of the workflow plan.
#' @examples
#' # Use target() to create your own custom columns in a drake plan.
#' # See ?triggers for more on triggers.
#' plan <- drake_plan(
#'   website_data = target(
#'     download_data("www.your_url.com"),
#'     trigger = "always",
#'     custom_column = 5
#'   ),
#'   analysis = analyze(website_data)
#' )
#' plan
#' # make(plan) # nolint
#' # Call target() inside or outside drake_plan().
#' target(
#'   download_data("www.your_url.com"),
#'   trigger = "always",
#'   custom_column = 5
#' )
target <- function(
  command = NULL,
  trigger = NULL,
  retries = NULL,
  timeout = NULL,
  cpu = NULL,
  elapsed = NULL,
  priority = NULL,
  worker = NULL,
  resources = NULL,
  ...
) {
  out <- list(
    command   = sanitize_cmd_type(rlang::enexpr(command)),
    trigger   = rlang::enexpr(trigger),
    retries   = rlang::enexpr(retries),
    timeout   = rlang::enexpr(timeout),
    cpu       = rlang::enexpr(cpu),
    elapsed   = rlang::enexpr(elapsed),
    priority  = rlang::enexpr(priority),
    worker    = rlang::enexpr(worker),
    resources = rlang::enexpr(resources)
  )
  out <- c(out, rlang::enexprs(...))
  out <- select_nonempty(out)
  out[nzchar(names(out))]
  out <- lapply(
    X = out,
    FUN = function(x) {
      if (is.language(x)) {
        wide_deparse(x)
      } else {
        x
      }
    }
  )
  weak_as_tibble(out)
}
