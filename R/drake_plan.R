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
#' - `evaluator`: An experimental column. Each entry is a function
#'   passed to the `evaluator` argument of `future::future()`
#'   for each worker in `make(..., parallelism = "future")`.
#'
#' @export
#' @seealso map_plan, reduce_by, gather_by, reduce_plan, gather_plan,
#'   evaluate_plan, expand_plan
#' @return A data frame of targets and commands. See the details
#' for optional columns you can append manually post-hoc.
#' @param ... A collection of symbols/targets
#'   with commands assigned to them. See the examples for details.
#' @param list A named character vector of commands
#'   with names as targets.
#' @param file_targets deprecated argument. See [file_out()],
#'   [file_in()], and [knitr_in()] for the current way to work
#'   with files.
#'   In the past, this argument was a logical to indicate whether the
#'   target names should be single-quoted to denote files. But the newer
#'   interface is much better.
#' @param strings_in_dots deprecated argument for handling strings in
#'   commands specified in the `...` argument. Defaults to `NULL` for backward
#'   compatibility. New code should use [file_out()], [file_in()], and
#'   [knitr_in()] to specify file names and set this argument to `"literals"`,
#'   which will at some point become the only accepted value.
#'
#'   To fully embrace the glorious new file API, call
#'   `pkgconfig::set_config("drake::strings_in_dots" = "literals")`
#'   right when you start your R session.
#'   That way, `drake` totally relies on [file_in()], [file_out()],
#'   and [knitr_in()] to coordinate input and output files, as
#'   opposed to deprecated features like single-quotes
#'   (and in the case of `knitr` reports,
#'   explicit calls to `knitr::knit()` and `rmarkdown::render()` in commands).
#'   This is why the default value of `strings_in_dots` is
#'   `pkgconfig::get_config("drake::strings_in_dots")`.
#'
#'   In the past, this argument was a character scalar denoting
#'   how to treat quoted character strings in the commands
#'   specified through `...`.
#'   Set to `"filenames"` to treat all these strings as
#'   external file targets/imports (single-quoted),
#'   or to `"literals"` to treat them all as literal
#'   strings (double-quoted).
#'   Unfortunately, because of how R deparses code,
#'   you cannot simply leave literal quotes alone in the
#'   `...` argument. R will either convert all these quotes
#'   to single quotes or double quotes. Literal quotes in the
#'   `list` argument are left alone.
#' @param tidy_evaluation logical, whether to use tidy evaluation
#'   such as quasiquotation
#'   when evaluating commands passed through the free-form
#'   `...` argument.
#' @examples
#' test_with_dir("Contain side effects", {
#' # Create workflow plan data frames.
#' mtcars_plan <- drake_plan(
#'   write.csv(mtcars[, c("mpg", "cyl")], file_out("mtcars.csv")),
#'   value = read.csv(file_in("mtcars.csv")),
#'   strings_in_dots = "literals"
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
#' # You can create your own custom columns too.
#' # See ?triggers for more on triggers.
#' drake_plan(
#'   website_data = target(
#'     command = download_data("www.your_url.com"),
#'     trigger = "always",
#'     custom_column = 5
#'   ),
#'   analysis = analyze(website_data),
#'   strings_in_dots = "literals"
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
  strings_in_dots = pkgconfig::get_config("drake::strings_in_dots"),
  tidy_evaluation = TRUE
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
      tibble(
        target = character(0),
        command = character(0)
      )
    )
  }
  commands <- complete_target_names(commands)
  targets <- names(commands)
  commands <- as.character(commands)
  plan <- tibble(
    target = targets,
    command = commands
  )
  from_dots <- plan$target %in% names(commands_dots)
  if (length(file_targets) || identical(strings_in_dots, "filenames")) {
    warning(
      "Use the file_in(), file_out(), and knitr_in() functions ",
      "to work with files in your commands, and pass ",
      "`strings_in_dots = \"literals\"`. ",
      "See `?drake_plan` for examples. ",
      "The `file_targets` argument is deprecated. ",
      "Worry about single-quotes no more!"
    )
  }
  if (identical(file_targets, TRUE)) {
    plan$target <- drake::drake_quotes(plan$target, single = TRUE)
  }

  # TODO: leave double-quoted strings alone when we're ready to
  # deprecate single-quoting in the file API.
  # Currently, to totally take advantage of the new file API,
  # users need to set strings_in_dots to "literals" every time.
  from_dots_with_quotes <- grep("\"", plan$command[from_dots])
  if (length(from_dots_with_quotes)) {
    if (!length(strings_in_dots)) {
      warning(
        "Converting double-quotes to single-quotes because ",
        "the `strings_in_dots` argument is missing. ",
        "Use the file_in(), file_out(), and knitr_in() functions ",
        "to work with files in your commands. To remove this warning, ",
        "either call `drake_plan()` with `strings_in_dots = \"literals\"` ",
        "or use ",
        "`pkgconfig::set_config(\"drake::strings_in_dots\" = \"literals\")`.",
        call. = FALSE
      )
    }
    if (!length(strings_in_dots) || identical(strings_in_dots, "filenames")) {
      plan$command[from_dots] <- gsub("\"", "'", plan$command[from_dots])
    }
  }
  plan <- parse_custom_columns(plan)
  sanitize_plan(plan)
}

#' @title Row-bind together drake plans
#' @description combine drake plans together in a way that
#'   correctly fills in missing entries.
#' @export
#' @seealso drake_plan, make
#' @param ... workflow plan data frames (see [drake_plan()])
#' @examples
#' # You might need to refresh your data regularly (see ?triggers).
#' download_plan <- drake_plan(
#'   data = target(
#'     command = download_data(),
#'     trigger = "always"
#'   ),
#'   strings_in_dots = "literals"
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
  plan <- dplyr::bind_rows(...)
  sanitize_plan(plan)
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

parse_custom_columns <- function(plan) {
  . <- NULL # For warnings about undefined global symbols.
  out <- dplyr::group_by(plan, seq_len(n()))
  out <- dplyr::do(out, parse_custom_colums_row(.))
  out[["seq_len(n())"]] <- NULL
  out
}

parse_custom_colums_row <- function(row) {
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

drake_plan_override <- function(target, field, config) {
  in_plan <- config$plan[[field]]
  if (is.null(in_plan)) {
    return(config[[field]])
  } else {
    # Should be length 0 or 1 because sanitize_plan()
    # already screens for duplicate target names.
    index <- which(config$plan$target == target)
    if (!length(index)) {
      stop("target ", target, " is not in the workflow plan.")
    }
    out <- in_plan[[index]]
    if (safe_is_na(out)) {
      out <- config[[field]]
    }
    out
  }
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
#'     contents = read.csv(file_in("mtcars.csv")),
#'     strings_in_dots = "literals" # deprecated but useful: no single quotes needed. # nolint
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
#'     contents = read.csv(file_in("mtcars.csv")),
#'     strings_in_dots = "literals" # deprecated but useful: no single quotes needed. # nolint
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
#' @return the argument
#' @param x code to ignore
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
  check_these <- purrr::map_lgl(names(dots), function(x) {
    !nzchar(x)
  })
  check_these <- which(check_these)
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
#' @param command the command to build the target
#' @param trigger the target's trigger
#' @param retries number of retries in case of failure
#' @param timeout overall timeout (in seconds) for building a target
#' @param cpu cpu timeout (seconds) for building a target
#' @param elapsed elapsed time (seconds) for building a target
#' @param priority integer giving the build priority of a target.
#'   Given two targets about to be built at the same time,
#'   the one with the lesser priority (numerically speaking)
#'   will be built first.
#' @param worker the preferred worker to be assigned the target
#'   (in parallel computing).
#' @param evaluator the `future` evaluator of the target.
#'   Not yet supported.
#' @param ... named arguments specifying non-standard
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
#'   analysis = analyze(website_data),
#'   strings_in_dots = "literals"
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
  evaluator = NULL,
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
    evaluator = rlang::enexpr(evaluator)
  )
  out <- c(out, rlang::enexprs(...))
  out <- select_nonempty(out)
  out[nzchar(names(out))]
  out <- purrr::map(
    .x = out,
    .f = function(x) {
      if (is.language(x)) {
        wide_deparse(x)
      } else {
        x
      }
    }
  )
  tibble::as_tibble(out)
}
