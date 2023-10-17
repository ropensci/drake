#' @title Customize a target in [drake_plan()].
#' `r lifecycle::badge("stable")`
#' @description The `target()` function is a way to
#'   configure individual targets in a `drake` plan.
#'   Its most common use is to invoke static branching
#'   and dynamic branching, and it can also set the values
#'   of custom columns such as `format`, `elapsed`, `retries`,
#'   and `max_expand`. Details are at
#'   `https://books.ropensci.org/drake/plans.html#special-columns`.
#'   Note: `drake_plan(my_target = my_command())`
#'   is equivalent to
#'   `drake_plan(my_target = target(my_command())`.
#' @details `target()` must be called inside [drake_plan()].
#'   It is invalid otherwise.
#' @export
#' @inheritSection drake_plan Columns
#' @inheritSection drake_plan Keywords
#' @inheritSection drake_plan Formats
#' @seealso [drake_plan()], [make()]
#' @return A one-row workflow plan data frame with the named
#' arguments as columns.
#' @param command The command to build the target.
#' @param transform A call to [map()], [split()], [cross()], or [combine()]
#'   to apply a *static* transformation. Details:
#'   `https://books.ropensci.org/drake/static.html`
#' @param dynamic A call to [map()], [cross()], or [group()]
#'   to apply a *dynamic* transformation. Details:
#'   `https://books.ropensci.org/drake/dynamic.html`
#' @param ... Optional columns of the plan for a given target.
#'   See the Columns section of this help file for a selection
#'   of special columns that `drake` understands.
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
#' models <- c("glm", "hierarchical")
#' plan <- drake_plan(
#'   data = target(
#'     get_data(x),
#'     transform = map(x = c("simulated", "survey"))
#'   ),
#'   analysis = target(
#'     analyze_data(data, model),
#'     transform = cross(data, model = !!models, .id = c(x, model))
#'   ),
#'   summary = target(
#'     summarize_analysis(analysis),
#'     transform = map(analysis, .id = c(x, model))
#'   ),
#'   results = target(
#'     bind_rows(summary),
#'     transform = combine(summary, .by = data)
#'   )
#' )
#' plan
#' if (requireNamespace("styler", quietly = TRUE)) {
#'   print(drake_plan_source(plan))
#' }
target <- function(
  command = NULL,
  transform = NULL,
  dynamic = NULL,
  ...
) {
  if (!nzchar(Sys.getenv("drake_target_silent"))) {
    # 2019-12-05
    stop0(
      "target() in drake is not a standalone user-side function. ",
      "It must be called from inside drake_plan(). Details: ",
      "https://books.ropensci.org/drake/static.html"
    )
  }
  call <- match.call(expand.dots = FALSE)
  lst <- c(
    command = call$command,
    transform = call$transform,
    dynamic = call$dynamic,
    call$...
  )
  lst <- select_nonempty(lst)
  lst <- lst[nzchar(names(lst))]
  out <- data.frame(command = NA, stringsAsFactors = FALSE)
  for (col in names(lst)) {
    if (is.language(lst[[col]])) {
      out[[col]] <- list(lst[[col]])
    } else {
      out[[col]] <- lst[[col]]
    }
  }
  out
}

#' @title Customize the decision rules for rebuilding targets
#' `r lifecycle::badge("stable")`
#' @description  Use this function inside a target's command
#'   in your [drake_plan()] or the `trigger` argument to
#'   [make()] or [drake_config()].
#'   For details, see the chapter on triggers
#'   in the user manual:
#'   `https://books.ropensci.org/drake/triggers.html`
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
#'   `https://books.ropensci.org/drake/plans.html#special-data-formats-for-targets` # nolint
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

#' @title Declare input files and directories.
#' `r lifecycle::badge("stable")`
#' @description `file_in()` marks individual files
#'   (and whole directories) that your targets depend on.
#' @section URLs:
#'   As of `drake` 7.4.0, `file_in()` and `file_out()` have
#'   support for URLs. If the file name begins with
#'   "http://", "https://", or "ftp://", [make()] attempts
#'   to check the ETag to see if the data changed from last time.
#'   If no ETag can be found, `drake` simply uses the ETag
#'   from last [make()] and registers the file as unchanged
#'   (which prevents your workflow from breaking if you lose
#'   internet access). If your `file_in()` URLs require
#'   authentication, see the `curl_handles` argument of
#'   `make()` and `drake_config()` to learn how to supply credentials.
#'
#' @export
#' @inheritSection drake_plan Keywords
#' @seealso [file_out()], [knitr_in()], [ignore()], [no_deps()]
#' @return A character vector of declared input file or directory paths.
#' @param ... Character vector, paths to files and directories. Use
#'   `.id_chr` to refer to the current target by name. `.id_chr` is not
#'    limited to use in `file_in()` and `file_out()`.
#' @export
#' @examples
#' \dontrun{
#' isolate_example("contain side effects", {
#' # The `file_out()` and `file_in()` functions
#' # just takes in strings and returns them.
#' file_out("summaries.txt")
#' # Their main purpose is to orchestrate your custom files
#' # in your workflow plan data frame.
#' plan <- drake_plan(
#'   out = write.csv(mtcars, file_out("mtcars.csv")),
#'   contents = read.csv(file_in("mtcars.csv"))
#' )
#' plan
#' # drake knows "\"mtcars.csv\"" is the first target
#' # and a dependency of `contents`. See for yourself:
#'
#' make(plan)
#' file.exists("mtcars.csv")
#'
#' # You may use `.id_chr` inside `file_out()` and `file_in()`
#' # to refer  to the current target. This works inside
#' # static `map()`, `combine()`, `split()`, and `cross()`.
#'
#' plan <- drake::drake_plan(
#'   data = target(
#'     write.csv(data, file_out(paste0(.id_chr, ".csv"))),
#'     transform = map(data = c(airquality, mtcars))
#'   )
#' )
#' plan
#'
#' # You can also work with entire directories this way.
#' # However, in `file_out("your_directory")`, the directory
#' # becomes an entire unit. Thus, `file_in("your_directory")`
#' # is more appropriate for subsequent steps than
#' # `file_in("your_directory/file_inside.txt")`.
#' plan <- drake_plan(
#'   out = {
#'     dir.create(file_out("dir"))
#'     write.csv(mtcars, "dir/mtcars.csv")
#'   },
#'   contents = read.csv(file.path(file_in("dir"), "mtcars.csv"))
#' )
#' plan
#'
#' make(plan)
#' file.exists("dir/mtcars.csv")
#'
#' # See the connections that the file relationships create:
#' if (requireNamespace("visNetwork", quietly = TRUE)) {
#'   vis_drake_graph(plan)
#' }
#' })
#' }
file_in <- function(...) {
  as.character(c(...))
}

#' @title Declare output files and directories.
#' `r lifecycle::badge("stable")`
#' @description `file_out()` marks individual files
#'   (and whole directories) that your targets create.
#' @export
#' @inheritSection drake_plan Keywords
#' @seealso [file_in()], [knitr_in()], [ignore()], [no_deps()]
#' @return A character vector of declared output file or directory paths.
#' @inheritParams file_in
#' @export
#' @examples
#' \dontrun{
#' isolate_example("contain side effects", {
#' # The `file_out()` and `file_in()` functions
#' # just takes in strings and returns them.
#' file_out("summaries.txt")
#' # Their main purpose is to orchestrate your custom files
#' # in your workflow plan data frame.
#' plan <- drake_plan(
#'   out = write.csv(mtcars, file_out("mtcars.csv")),
#'   contents = read.csv(file_in("mtcars.csv"))
#' )
#' plan
#' # drake knows "\"mtcars.csv\"" is the first target
#' # and a dependency of `contents`. See for yourself:
#'
#' make(plan)
#' file.exists("mtcars.csv")
#'
#'  # You may use `.id_chr` inside `file_out()` and `file_in()`
#'  # to refer  to the current target. This works inside `map()`,
#'  # `combine()`, `split()`, and `cross()`.
#'
#' plan <- drake::drake_plan(
#'   data = target(
#'     write.csv(data, file_out(paste0(.id_chr, ".csv"))),
#'     transform = map(data = c(airquality, mtcars))
#'   )
#' )
#'
#' plan
#'
#' # You can also work with entire directories this way.
#' # However, in `file_out("your_directory")`, the directory
#' # becomes an entire unit. Thus, `file_in("your_directory")`
#' # is more appropriate for subsequent steps than
#' # `file_in("your_directory/file_inside.txt")`.
#' plan <- drake_plan(
#'   out = {
#'     dir.create(file_out("dir"))
#'     write.csv(mtcars, "dir/mtcars.csv")
#'   },
#'   contents = read.csv(file.path(file_in("dir"), "mtcars.csv"))
#' )
#' plan
#'
#' make(plan)
#' file.exists("dir/mtcars.csv")
#'
#' # See the connections that the file relationships create:
#' if (requireNamespace("visNetwork", quietly = TRUE)) {
#'   vis_drake_graph(plan)
#' }
#' })
#' }
file_out <- file_in

#' @title Declare `knitr`/`rmarkdown` source files
#'   as dependencies.
#' `r lifecycle::badge("stable")`
#' @description `knitr_in()` marks individual `knitr`/R Markdown
#'   reports as dependencies. In `drake`, these reports are pieces
#'   of the pipeline. R Markdown is a great tool for *displaying*
#'   precomputed results, but not for running a large workflow
#'   from end to end. These reports should do as little
#'   computation as possible.
#' @details Unlike [file_in()] and [file_out()], `knitr_in()`
#'   does not work with entire directories.
#' @export
#' @inheritSection drake_plan Keywords
#' @seealso [file_in()], [file_out()], [ignore()], [no_deps()]
#' @return A character vector of declared input file paths.
#' @param ... Character strings. File paths of `knitr`/`rmarkdown`
#'   source files supplied to a command in your workflow plan data frame.
#' @examples
#' \dontrun{
#' isolate_example("contain side effects", {
#' if (requireNamespace("knitr", quietly = TRUE)) {
#' # `knitr_in()` is like `file_in()`
#' # except that it analyzes active code chunks in your `knitr`
#' # source file and detects non-file dependencies.
#' # That way, updates to the right dependencies trigger rebuilds
#' # in your report.
#' # The mtcars example (`drake_example("mtcars")`)
#' # already has a demonstration
#'
#' load_mtcars_example()
#' make(my_plan)
#'
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

#' @title Ignore code
#' `r lifecycle::badge("stable")`
#' @description Ignore sections of commands and imported functions.
#' @details In user-defined functions and [drake_plan()] commands, you can
#' wrap code chunks in `ignore()` to
#' 1. Tell `drake` to not search for dependencies
#'   (targets etc. mentioned in the code) and
#' 2. Ignore changes to the code so downstream targets remain up to date.
#' To enforce (1) without (2), use [no_deps()].
#' @export
#' @inheritSection drake_plan Keywords
#' @seealso [file_in()], [file_out()], [knitr_in()], [no_deps()]
#' @return The argument.
#' @param x Code to ignore.
#' @examples
#' \dontrun{
#' isolate_example("Contain side effects", {
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
#'
#' # ignore() works with functions and multiline code chunks.
#' f <- function(x) {
#'   ignore({
#'     x <- x + 1
#'     x <- x + 2
#'   })
#'   x # Not ignored.
#' }
#' make(plan = drake_plan(y = f(2)))
#' readd(x)
#' # Changes the content of the ignore() block:
#' f <- function(x) {
#'   ignore({
#'     x <- x + 1
#'   })
#'   x # Not ignored.
#' }
#' make(plan = drake_plan(x = f(2)))
#' readd(x)
#' })
#' }
ignore <- function(x = NULL) {
  x
}

#' @title Suppress dependency detection.
#' `r lifecycle::badge("stable")`
#' @description Tell `drake` to not search for dependencies in a chunk of code.
#' @details `no_deps()` is similar to [ignore()], but it still lets `drake`
#'   track meaningful changes to the code itself.
#' @export
#' @inheritSection drake_plan Keywords
#' @seealso [file_in()], [file_out()], [knitr_in()], [no_deps()]
#' @return The argument.
#' @param x Code for which dependency detection is suppressed.
#' @examples
#' \dontrun{
#' isolate_example("Contain side effects", {
#' # Normally, `drake` reacts to changes in dependencies.
#' x <- 4
#' make(plan = drake_plan(y = sqrt(x)))
#' x <- 5
#' make(plan = drake_plan(y = sqrt(x)))
#' make(plan = drake_plan(y = sqrt(4) + x))
#' # But not with no_deps().
#' make(plan = drake_plan(y = sqrt(4) + no_deps(x))) # Builds y.
#' x <- 6
#' make(plan = drake_plan(y = sqrt(4) + no_deps(x))) # Skips y.
#' # However, `drake` *does* react to changes
#' # to the *literal code* inside `no_deps()`.
#' make(plan = drake_plan(y = sqrt(4) + ignore(x + 1))) # Builds y.
#'
#' # Like ignore(), no_deps() works with functions and multiline code chunks.
#' z <- 1
#' f <- function(x) {
#'   no_deps({
#'     x <- z + 1
#'     x <- x + 2
#'   })
#'   x
#' }
#' make(plan = drake_plan(y = f(2)))
#' readd(y)
#' z <- 2 # Changed dependency is not tracked.
#' make(plan = drake_plan(y = f(2)))
#' readd(y)
#' })
#' }
no_deps <- function(x = NULL) {
  x
}

#' @title Cancel a target mid-build under some condition
#'   `r lifecycle::badge("stable")`
#' @description Cancel a target mid-build if some logical condition is met.
#'   Upon cancellation, `drake` halts the current target and moves to the
#'   next one. The target's previous value and metadata, if they exist,
#'   remain in the cache.
#' @export
#' @seealso cancel
#' @return Nothing.
#' @inheritParams cancel
#' @param condition Logical, whether to cancel the target.
#' @examples
#' \dontrun{
#' isolate_example("cancel_if()", {
#' f <- function(x) {
#'   cancel_if(x > 1)
#'   Sys.sleep(2) # Does not run if x > 1.
#' }
#' g <- function(x) f(x)
#' plan <- drake_plan(y = g(2))
#' make(plan)
#' # Does not exist.
#' # readd(y)
#' })
#' }
cancel_if <- function(condition, allow_missing = TRUE) {
  envir_call()
  if (length(condition) != 1L) {
    stop("condition must be length 1 in cancel_if()")
  }
  if (!condition) {
    return(invisible())
  }
  cancel(allow_missing = allow_missing)
}

#' @title Cancel a target mid-build `r lifecycle::badge("stable")`
#' @description Cancel a target mid-build.
#'   Upon cancellation, `drake` halts the current target and moves to the
#'   next one. The target's previous value and metadata, if they exist,
#'   remain in the cache.
#' @export
#' @seealso cancel_if
#' @return Nothing.
#' @param allow_missing Logical. If `FALSE`, `drake` will not cancel
#'   the target if it is missing from the cache (or if you removed the
#'   key with `clean()`).
#' @examples
#' \dontrun{
#' isolate_example("cancel()", {
#' f <- function(x) {
#'   cancel()
#'   Sys.sleep(2) # Does not run.
#' }
#' g <- function(x) f(x)
#' plan <- drake_plan(y = g(1))
#' make(plan)
#' # Does not exist.
#' # readd(y)
#' })
#' }
cancel <- function(allow_missing = TRUE) {
  envir <- envir_call()
  if (!allow_missing) {
    if (target_missing(envir$target, envir$config)) {
      return(invisible())
    }
  }
  stop(cancellation(allow_missing = allow_missing))
}

cancellation <- function(...) {
  structure(
    list(message = "cancel", call = NULL),
    class = c("drake_cancel", "error", "condition")
  )
}

#' @title Name of the current target `r lifecycle::badge("stable")`
#' @export
#' @description `id_chr()` gives you the name of the current target
#'   while [make()] is running. For static branching in [drake_plan()],
#'   use the `.id_chr` symbol instead. See the examples for details.
#' @inheritSection drake_plan Keywords
#' @return The name of the current target.
#' @examples
#' try(id_chr()) # Do not use outside the plan.
#' \dontrun{
#' isolate_example("id_chr()", {
#' plan <- drake_plan(x = id_chr())
#' make(plan)
#' readd(x)
#' # Dynamic branching
#' plan <- drake_plan(
#'   x = seq_len(4),
#'   y = target(id_chr(), dynamic = map(x))
#' )
#' make(plan)
#' readd(y, subtargets = 1)
#' # Static branching
#' plan <- drake_plan(
#'   y = target(c(x, .id_chr), transform = map(x = !!seq_len(4)))
#' )
#' plan
#' })
#' }
id_chr <- function() {
  envir_call()$target
}

#' @title Get the environment where drake builds targets
#' `r lifecycle::badge("questioning")`
#' @description Call this function inside the commands in your plan
#'   to get the environment where `drake` builds targets.
#'   Advanced users can use it to strategically remove targets from memory
#'   while [make()] is running.
#' @details `drake` manages in-memory targets in 4 environments:
#'   one with sub-targets, one with whole dynamic targets, one with
#'   static targets, and one with imported global objects and functions.
#'   This last environment is usually the environment
#'   from which you call [make()].
#'   Select the appropriate environment for your
#'   use case with the `which` argument of `drake_envir()`.
#' @export
#' @inheritSection drake_plan Keywords
#' @seealso [from_plan()]
#' @return The environment where `drake` builds targets.
#' @param which Character of length 1, which environment
#'   to select. See the details of this help file.
#' @examples
#' \dontrun{
#' isolate_example("contain side effects", {
#' plan <- drake_plan(
#'   large_data_1 = sample.int(1e4),
#'   large_data_2 = sample.int(1e4),
#'   subset = c(large_data_1[seq_len(10)], large_data_2[seq_len(10)]),
#'   summary = {
#'     print(ls(envir = parent.env(drake_envir())))
#'     # We don't need the large_data_* targets in memory anymore.
#'     rm(large_data_1, large_data_2, envir = drake_envir("targets"))
#'     print(ls(envir = drake_envir("targets")))
#'     mean(subset)
#'   }
#' )
#' make(plan, cache = storr::storr_environment(), session_info = FALSE)
#' })
#' }
drake_envir <- function(
  which = c("targets", "dynamic", "subtargets", "imports")
) {
  config <- envir_call()$config
  which <- match.arg(which)
  which <- ifelse(which == "imports", "envir", paste0("envir_", which))
  config[[which]]
}

envir_call <- function() {
  calls <- vcapply(sys.calls(), safe_deparse)
  matching_calls <- grepl("^drake_with_call_stack_8a6af5\\(", calls)
  if (!any(matching_calls)) {
    envir_call_error()
  }
  pos <- max(which(matching_calls))
  sys.frame(pos)
}

envir_call_error <- function() {
  stop0(
    "Could not find the environment where drake builds targets. ",
    "Functions drake_envir(), id_chr(), cancel(), and cancel_if() ",
    "can only be invoked through make()."
  )
}

#' @title Row-bind together drake plans
#' `r lifecycle::badge("stable")`
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
bind_plans <- function(...) {
  sanitize_plan(drake_bind_rows(...))
}

#' @title Turn an R script file or `knitr` / R Markdown report
#'   into a `drake` plan.
#' `r lifecycle::badge("questioning")`
#' @export
#' @seealso [drake_plan()], [make()], [plan_to_code()],
#'   [plan_to_notebook()]
#' @description `code_to_plan()`, [plan_to_code()], and
#'   [plan_to_notebook()] together illustrate the relationships
#'   between `drake` plans, R scripts, and R Markdown documents.
#' @details This feature is easy to break, so there are some rules
#'   for your code file:
#'   1. Stick to assigning a single expression to a single target at a time.
#'     For multi-line commands, please enclose the whole command
#'     in curly braces.
#'     Conversely, compound assignment is not supported
#'     (e.g. `target_1 <- target_2 <- target_3 <- get_data()`).
#'   2. Once you assign an expression to a variable,
#'     do not modify the variable any more.
#'     The target/command binding should be permanent.
#'   3. Keep it simple. Please use the assignment operators rather than
#'     `assign()` and similar functions.
#' @param path A file path to an R script or `knitr` report.
#' @examples
#' plan <- drake_plan(
#'   raw_data = read_excel(file_in("raw_data.xlsx")),
#'   data = raw_data,
#'   hist = create_plot(data),
#'   fit = lm(Ozone ~ Temp + Wind, data)
#' )
#' file <- tempfile()
#' # Turn the plan into an R script a the given file path.
#' plan_to_code(plan, file)
#' # Here is what the script looks like.
#' cat(readLines(file), sep = "\n")
#' # Convert back to a drake plan.
#' code_to_plan(file)
code_to_plan <- function(path) {
  stopifnot(file.exists(path))
  txt <- readLines(path)
  # From CodeDepends: https://github.com/duncantl/CodeDepends/blob/7c9cf7eceffaea1d26fe25836c7a455f059e13c1/R/frags.R#L74 # nolint
  # Checks if the file is a knitr report.
  if (any(grepl(knitr_pattern, txt))) { # nolint
    txt <- get_tangled_text(path)
  }
  nodes <- parse(text = txt)
  out <- lapply(nodes, node_plan)
  out <- do.call(rbind, out)
  out <- parse_custom_plan_columns(out)
  sanitize_plan(out)
}

node_plan <- function(node) {
  weak_tibble(
    target = safe_deparse(node[[2]], backtick = FALSE),
    command = safe_deparse(node[[3]], backtick = TRUE)
  )
}

#' @title Turn a `drake` plan into a plain R script file.
#' `r lifecycle::badge("questioning")`
#' @export
#' @seealso [drake_plan()], [make()], [code_to_plan()],
#'   [plan_to_notebook()]
#' @description `code_to_plan()`, [plan_to_code()], and
#'   [plan_to_notebook()] together illustrate the relationships
#'   between `drake` plans, R scripts, and R Markdown documents.
#'   In the file generated by `plan_to_code()`, every target/command pair
#'   becomes a chunk of code.
#'   Targets are arranged in topological order
#'   so dependencies are available before their downstream targets.
#'   Please note:
#'   1. You are still responsible for loading your project's
#'     packages, imported functions, etc.
#'   2. Triggers disappear.
#' @param plan Workflow plan data frame. See [drake_plan()]
#'   for details.
#' @param con A file path or connection to write to.
#' @examples
#' plan <- drake_plan(
#'   raw_data = read_excel(file_in("raw_data.xlsx")),
#'   data = raw_data,
#'   hist = create_plot(data),
#'   fit = lm(Ozone ~ Temp + Wind, data)
#' )
#' file <- tempfile()
#' # Turn the plan into an R script a the given file path.
#' plan_to_code(plan, file)
#' # Here is what the script looks like.
#' cat(readLines(file), sep = "\n")
#' # Convert back to a drake plan.
#' code_to_plan(file)
plan_to_code <- function(plan, con = stdout()) {
  writeLines(text = plan_to_text(plan), con = con)
}

#' @title Turn a `drake` plan into an R notebook.
#' `r lifecycle::badge("questioning")`
#' @export
#' @seealso [drake_plan()], [make()], [code_to_plan()],
#'   [plan_to_code()]
#' @description `code_to_plan()`, [plan_to_code()], and
#'   [plan_to_notebook()] together illustrate the relationships
#'   between `drake` plans, R scripts, and R Markdown documents.
#'   In the file generated by `plan_to_code()`, every target/command pair
#'   becomes a chunk of code.
#'   Targets are arranged in topological order
#'   so dependencies are available before their downstream targets.
#'   Please note:
#'   1. You are still responsible for loading your project's
#'     packages, imported functions, etc.
#'   2. Triggers disappear.
#' @inheritParams plan_to_code
#' @examples
#' if (suppressWarnings(require("knitr"))) {
#' plan <- drake_plan(
#'   raw_data = read_excel(file_in("raw_data.xlsx")),
#'   data = raw_data,
#'   hist = create_plot(data),
#'   fit = lm(Ozone ~ Temp + Wind, data)
#' )
#' file <- tempfile()
#' # Turn the plan into an R notebook a the given file path.
#' plan_to_notebook(plan, file)
#' # Here is what the script looks like.
#' cat(readLines(file), sep = "\n")
#' # Convert back to a drake plan.
#' code_to_plan(file)
#' }
plan_to_notebook <- function(plan, con) {
  out <- c(
    "---",
    "title: \"My Notebook\"",
    "output: html_notebook",
    "---",
    "",
    "```{r my_code}",
    plan_to_text(plan),
    "```"
  )
  writeLines(out, con = con)
}

plan_to_text <- function(plan) {
  . <- NULL
  graph <- drake_config(
    plan[, c("target", "command")],
    envir = new.env(parent = emptyenv()),
    cache = storr::storr_environment(),
    history = FALSE,
    verbose = FALSE
  )$graph
  order <- igraph::topo_sort(graph)$name
  order <- intersect(order, plan$target)
  order <- match(order, table = plan$target)
  plan <- plan[order, ]
  if (!is.character(plan$command)) {
    plan$command <- vapply(
      plan$command,
      safe_deparse,
      FUN.VALUE = character(1),
      backtick = TRUE
    )
  }
  text <- paste(plan$target, "<-", plan$command)
  if (requireNamespace("styler")) {
    try(text <- styler::style_text(text), silent = TRUE)
  }
  text
}

#' @title Show the code required to produce a given `drake` plan
#' `r lifecycle::badge("stable")`
#' @description You supply a plan, and [drake_plan_source()]
#'   supplies code to generate that plan. If you have the
#'   `prettycode` package,
#'   installed, you also get nice syntax highlighting in the console
#'   when you print it.
#' @export
#' @seealso [drake_plan()]
#' @return a character vector of lines of text. This text
#'   is a call to [drake_plan()] that produces the plan you provide.
#' @param plan A workflow plan data frame (see [drake_plan()])
#' @examples
#' plan <- drake::drake_plan(
#'   small_data = download_data("https://some_website.com"),
#'   large_data_raw = target(
#'     command = download_data("https://lots_of_data.com"),
#'     trigger = trigger(
#'       change = time_last_modified("https://lots_of_data.com"),
#'       command = FALSE,
#'       depend = FALSE
#'     ),
#'     timeout = 1e3
#'   )
#' )
#' print(plan)
#' if (requireNamespace("styler", quietly = TRUE)) {
#'   source <- drake_plan_source(plan)
#'   print(source) # Install the prettycode package for syntax highlighting.
#'   file <- tempfile() # Path to an R script to contain the drake_plan() call.
#'   writeLines(source, file) # Save the code to an R script.
#' }
drake_plan_source <- function(plan) {
  assert_pkg("styler")
  text <- drake_plan_call(plan)
  text <- style_recursive(text, name = "", append_comma = FALSE)
  class(text) <- c("drake_plan_source", "character")
  text
}

#' @export
print.drake_plan_source <- function(x, ...) {
  if (requireNamespace("prettycode", quietly = TRUE)) {
    x <- prettycode::highlight(x)
  }
  cat(x, sep = "\n")
}

drake_plan_call <- function(plan) {
  target_calls <- drake_pmap(plan, drake_target_call)
  names(target_calls) <- plan$target
  as.call(c(quote(drake_plan), target_calls))
}

drake_target_call <- function(...) {
  args <- select_valid_args(list(...))
  target <- parse(text = args$target)[[1]]
  args$target <- NULL
  if (is.character(args[["command"]])) {
    args$command <- safe_parse(args[["command"]])
  }
  if ("trigger" %in% names(args)) {
    if (is.character(args[["trigger"]])) {
      args[["trigger"]] <- safe_parse(args[["trigger"]])
    }
  }
  if (!identical(names(args), "command")) {
    args[["command"]] <- as.call(c(quote(target), args))
  }
  args[["command"]]
}

select_valid_args <- function(x) {
  index <- vapply(
    X = x,
    FUN = function(y) {
      length(y) > 0 && !safe_is_na(y)
    },
    FUN.VALUE = logical(1)
  )
  x[index]
}

style_recursive <- function(expr, name, append_comma) {
  text <- style_recursive_loop(expr)
  head <- character(0)
  if (nzchar(name)) {
    head <- paste(name, "= ")
  }
  head <- paste0(head, safe_deparse(expr[[1]], backtick = FALSE), "(")
  out <- c(head, paste0("  ", text), ")")
  if (append_comma) {
    out[length(out)] <- paste0(out[length(out)], ",")
  }
  out
}

style_recursive_loop <- function(expr) {
  args <- expr[-1]
  text <- character(0)
  for (i in seq_along(args)) {
    recurse <- is_target_call(args[[i]]) || is_trigger_call(args[[i]])
    if (recurse) {
      text <- c(
        text,
        style_recursive(
          expr = args[[i]],
          name = names(args)[i],
          append_comma = i < length(args)
        )
      )
    } else {
      text <- c(
        text,
        style_leaf(
          expr = args[[i]],
          name = names(args)[i],
          append_comma = i < length(args)
        )
      )
    }
  }
  text
}

style_leaf <- function(name, expr, append_comma) {
  text <- safe_deparse(expr, backtick = TRUE)
  try(text <- styler::style_text(text), silent = TRUE)
  text[1] <- paste(name, "=", text[1])
  if (append_comma) {
    text[length(text)] <- paste0(text[length(text)], ",")
  }
  text
}

is_trigger_call <- function(expr) {
  tryCatch(
    safe_deparse(expr[[1]], backtick = FALSE) %in% trigger_fns,
    error = error_false
  )
}

#' @title Turn a script into a function.
#' `r lifecycle::badge("stable")`
#' @description `code_to_function()` is a quick (and very dirty) way to
#'   retrofit drake to an existing script-based project. It parses
#'   individual `\*.R/\*.RMD` files into functions so they can be added
#'   into the drake workflow.
#'
#' @details Most data science workflows consist of imperative scripts.
#'   `drake`, on the other hand, assumes you write *functions*.
#'   `code_to_function()` allows for pre-existing workflows to incorporate
#'   drake as a workflow management tool seamlessly for cases where
#'   re-factoring is unfeasible. So drake can monitor dependencies, the
#'   targets are passed as arguments of the dependent functions.
#'
#' @export
#' @seealso [file_in()], [file_out()], [knitr_in()], [ignore()], [no_deps()],
#' [code_to_plan()], [plan_to_code()], [plan_to_notebook()]
#' @return A function to be input into the drake plan
#' @param path Character vector, path to script.
#' @param envir Environment of the created function.
#' @export
#' @examples
#' \dontrun{
#' isolate_example("contain side effects", {
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#' # The `code_to_function()` function creates a function that makes it
#' # available for drake to process as part of the workflow.
#' # The main purpose is to allow pre-existing workflows to incorporate drake
#' # into the workflow seamlessly for cases where re-factoring is unfeasible.
#' #
#'
#' script1 <- tempfile()
#' script2 <- tempfile()
#' script3 <- tempfile()
#' script4 <- tempfile()
#'
#' writeLines(c(
#'   "data <- mtcars",
#'   "data$make <- do.call('c',",
#'   "lapply(strsplit(rownames(data), split=\" \"), `[`, 1))",
#'   "saveRDS(data, \"mtcars_alt.RDS\")"
#'  ),
#'   script1
#' )
#'
#' writeLines(c(
#'   "data <- readRDS(\"mtcars_alt.RDS\")",
#'   "mtcars_lm <- lm(mpg~cyl+disp+vs+gear+make,data=data)",
#'   "saveRDS(mtcars_lm, \"mtcars_lm.RDS\")"
#'   ),
#'   script2
#' )
#' writeLines(c(
#'   "mtcars_lm <- readRDS(\"mtcars_lm.RDS\")",
#'   "lm_summary <- summary(mtcars_lm)",
#'   "saveRDS(lm_summary, \"mtcars_lm_summary.RDS\")"
#'   ),
#'   script3
#' )
#' writeLines(c(
#'   "data<-readRDS(\"mtcars_alt.RDS\")",
#'   "gg <- ggplot2::ggplot(data)+",
#'   "ggplot2::geom_point(ggplot2::aes(",
#'   "x=disp, y=mpg, shape=as.factor(vs), color=make))",
#'   "ggplot2::ggsave(\"mtcars_plot.png\", gg)"
#'  ),
#'   script4
#' )
#'
#'
#' do_munge <- code_to_function(script1)
#' do_analysis <- code_to_function(script2)
#' do_summarize <- code_to_function(script3)
#' do_vis <- code_to_function(script4)
#'
#' plan <- drake_plan(
#'   munged   = do_munge(),
#'   analysis = do_analysis(munged),
#'   summary  = do_summarize(analysis),
#'   plot     = do_vis(munged)
#'  )
#'
#' plan
#' # drake knows  "script1" is the first script to be evaluated and ran,
#' # because it has no dependencies on other code and a dependency of
#' # `analysis`. See for yourself:
#'
#' make(plan)
#'
#' # See the connections that the sourced scripts create:
#' if (requireNamespace("visNetwork", quietly = TRUE)) {
#'   vis_drake_graph(plan)
#' }
#' }
#' })
#' }
code_to_function <- function(path, envir = parent.frame()) {
  force(envir)
  lines <- readLines(path)
  if (any(grepl(knitr_pattern, lines))) {
    lines <- get_tangled_text(path)
  }
  lines <- c(
    "function(...) {",
    lines,
    "list(time = Sys.time(),tempfile = tempfile())",
    "}"
  )
  text <- paste(lines, sep = "\n")
  func <- eval(safe_parse(text), envir = envir)
  func
}

knitr_pattern <- "^(### chunk number|<<[^>]*>>=|```\\{r.*\\})" # nolint
