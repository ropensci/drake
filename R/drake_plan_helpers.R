#' @title Customize a target in [drake_plan()].
#' \lifecycle{maturing}
#' @description Must be called inside [drake_plan()].
#'   Invalid otherwise.
#' @export
#' @inheritSection drake_plan Columns
#' @inheritSection drake_plan Keywords
#' @seealso [drake_plan()], [make()]
#' @return A one-row workflow plan data frame with the named
#' arguments as columns.
#' @param command The command to build the target.
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

#' @title Define multiple targets at once
#' \lifecycle{maturing}
#' @description Similar to `pmap()` from `purrr`, except `drake`'s
#'   `map()` defines new targets.
#' @details Only valid within a call to [target()] in
#'   [drake_plan()]. See the examples below.
#' @inheritSection drake_plan Keywords
#' @seealso split, cross, combine, drake_plan, target
#' @param ... Grouping variables. New grouping variables must be
#'   supplied with their names and values, existing grouping variables
#'   can be given as symbols without any values assigned.
#' @param .data A data frame of new grouping variables with
#'   grouping variable names as column names and values as elements.
#' @param .id Symbol or vector of symbols naming grouping variables
#'   to incorporate into target names. Useful for creating short target
#'   names. Set `.id = FALSE` to use integer indices as target name suffixes.
#' @param .tag_in A symbol or vector of symbols. Tags assign targets
#'   to grouping variables. Use `.tag_in` to assign *untransformed*
#'   targets to grouping variables.
#' @param .tag_out Just like `.tag_in`, except that `.tag_out`
#'   assigns *transformed* targets to grouping variables.
#' @examples
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
#' if (requireNamespace("styler")) {
#'   print(drake_plan_source(plan))
#' }
#' # Tags:
#' drake_plan(
#'   x = target(
#'     command,
#'     transform = map(y = c(1, 2), .tag_in = from, .tag_out = c(to, out))
#'   ),
#'   trace = TRUE
#' )
#' plan <- drake_plan(
#'   survey = target(
#'     survey_data(x),
#'     transform = map(x = c(1, 2), .tag_in = source, .tag_out = dataset)
#'   ),
#'   download = target(
#'     download_data(),
#'     transform = map(y = c(5, 6), .tag_in = source, .tag_out = dataset)
#'   ),
#'   analysis = target(
#'     analyze(dataset),
#'     transform = map(dataset)
#'   ),
#'   results = target(
#'     bind_rows(analysis),
#'     transform = combine(analysis, .by = source)
#'   )
#' )
#' plan
#' if (requireNamespace("styler", quietly = TRUE)) {
#'   print(drake_plan_source(plan))
#' }
map <- function(..., .data, .id, .tag_in, .tag_out) {
  stop(
    "map() in drake must be called inside target() in drake_plan()",
    call. = FALSE
  )
}

#' @title Define a target for each subset of data
#' \lifecycle{maturing}
#' @description Similar `group_map()`, from `dplyr`, except it
#'   defines new targets in `drake`.
#' @details Only valid within a call to [target()] in
#'   [drake_plan()]. See the examples below.
#' @inheritSection drake_plan Keywords
#' @seealso map, cross, combine, drake_plan, target, drake_slice
#' @inheritParams map
#' @inheritParams drake_slice
#' @examples
#' plan <- drake_plan(
#'   analysis = target(
#'     analyze(data),
#'     transform = split(data, slices = 3L, margin = 1L, drop = FALSE)
#'   )
#' )
#' print(plan)
#' if (requireNamespace("styler", quietly = TRUE)) {
#'   print(drake_plan_source(plan))
#' }
split <- function(..., .id, .tag_in, .tag_out) {
  stop(
    "split() in drake must be called inside target() in drake_plan()",
    call. = FALSE
  )
}

#' @title Define a target for each combination of values
#' \lifecycle{maturing}
#' @description Similar `crossing()`, from `tidyr`, except it
#'   defines new targets in `drake`.
#' @details Only valid within a call to [target()] in
#'   [drake_plan()]. See the examples below.
#' @inheritSection drake_plan Keywords
#' @seealso map, split, combine, drake_plan, target
#' @inheritParams map
#' @examples
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
cross <- function(..., .data, .id, .tag_in, .tag_out) {
  stop(
    "cross() in drake must be called inside target() in drake_plan()",
    call. = FALSE
  )
}

#' @title Define aggregates of other targets
#' \lifecycle{maturing}
#' @description Similar `summarize()`, from `dplyr`, except it
#'   defines new targets in `drake`.
#' @details Only valid within a call to [target()] in
#'   [drake_plan()]. See the examples below.
#' @inheritSection drake_plan Keywords
#' @seealso map, split, cross, drake_plan, target
#' @inheritParams map
#' @param .by Symbol or vector of symbols of grouping variables.
#'   `combine()` aggregates/groups targets by the grouping variables
#'   in `.by`
#' @examples
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
combine <- function(..., .by, .id, .tag_in, .tag_out) {
  stop(
    "combine() in drake must be called inside target() in drake_plan()",
    call. = FALSE
  )
}

#' @title Customize the decision rules for rebuilding targets
#' \lifecycle{stable}
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
#' @param command Logical, whether to rebuild the target if the
#'   [drake_plan()] command changes.
#' @param depend Logical, whether to rebuild if a
#'   non-file dependency changes.
#' @param file Logical, whether to rebuild the target
#'   if a [file_in()]/[file_out()]/[knitr_in()] file changes.
#' @param seed Logical, whether to rebuild the target
#'   if the seed changes. Only makes a difference if you set
#'   a custom `seed` column in your [drake_plan()] at some point
#'   in your workflow.
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
    seed = seed,
    condition = rlang::quo_squash(rlang::enquo(condition)),
    change = rlang::quo_squash(rlang::enquo(change)),
    mode = match.arg(mode)
  )
}

#' @title Declare input files and directories.
#' \lifecycle{stable}
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
#' @param ... Character vector, paths to files and directories.
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
#' config <- drake_config(plan)
#' if (requireNamespace("visNetwork", quietly = TRUE)) {
#'   vis_drake_graph(config)
#' }
#' })
#' }
file_in <- function(...) {
  as.character(c(...))
}

#' @title Declare output files and directories.
#' \lifecycle{stable}
#' @description `file_out()` marks individual files
#'   (and whole directories) that your targets create.
#' @export
#' @inheritSection drake_plan Keywords
#' @seealso [file_out()], [knitr_in()], [ignore()], [no_deps()]
#' @return A character vector of declared output file or directory paths.
#' @param ... Character vector, paths to files and directories.
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
#' config <- drake_config(plan)
#' if (requireNamespace("visNetwork", quietly = TRUE)) {
#'   vis_drake_graph(config)
#' }
#' })
#' }
file_out <- file_in

#' @title Declare `knitr`/`rmarkdown` source files
#'   as dependencies.
#' \lifecycle{stable}
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
#' })
#' }
knitr_in <- file_in

#' @title Ignore code
#' \lifecycle{stable}
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
#' \lifecycle{stable}
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

#' @title Get the environment where drake builds targets
#' \lifecycle{questioning}
#' @description Call this function inside the commands in your plan
#'   to get the environment where `drake` builds targets.
#'   That way, you can strategically remove targets from memory
#'   while [make()] is running. That way, you can limit the
#'   amount of computer memory you use.
#' @export
#' @inheritSection drake_plan Keywords
#' @seealso [from_plan()]
#' @return The environment where `drake` builds targets.
#' @examples
#' \dontrun{
#' isolate_example("contain side effects", {
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
#' })
#' }
drake_envir <- function() {
  envir <- environment()
  for (i in seq_len(getOption("expressions"))) {
    if (exists(drake_envir_marker, envir = envir, inherits = FALSE)) {
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

drake_envir_marker <- "._drake_envir"
drake_target_marker <- "._drake_target"
drake_markers <- c(
  drake_envir_marker,
  drake_target_marker
)

#' @title Row-bind together drake plans
#' \lifecycle{stable}
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
#' \lifecycle{stable}
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
#'   fit = lm(Sepal.Width ~ Petal.Width + Species, data)
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
  if (any(grepl("^(### chunk number|<<[^>]*>>=|```\\{r.*\\})", txt))) { # nolint
    nodes <- get_tangled_frags(path)
  } else {
    nodes <- parse(text = txt)
  }
  out <- lapply(nodes, node_plan)
  out <- do.call(rbind, out)
  out <- parse_custom_plan_columns(out)
  sanitize_plan(out)
}

node_plan <- function(node) {
  weak_tibble(
    target = safe_deparse(node[[2]]),
    command = safe_deparse(node[[3]])
  )
}

#' @title Turn a `drake` plan into a plain R script file.
#' \lifecycle{stable}
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
#'   fit = lm(Sepal.Width ~ Petal.Width + Species, data)
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
#' \lifecycle{stable}
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
#'   fit = lm(Sepal.Width ~ Petal.Width + Species, data)
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
    plan$command <- vapply(plan$command,
                           safe_deparse,
                           FUN.VALUE = character(1))
  }
  text <- paste(plan$target, "<-", plan$command)
  if (requireNamespace("styler")) {
    text <- styler::style_text(text)
  }
  text
}

#' @title Show the code required to produce a given `drake` plan
#' \lifecycle{stable}
#' @description You supply a plan, and [drake_plan_source()]
#'   supplies code to generate that plan. If you have the
#'   [`prettycode` package](https://github.com/r-lib/prettycode),
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
  head <- paste0(head, safe_deparse(expr[[1]]), "(")
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
  text <- styler::style_text(safe_deparse(expr))
  text[1] <- paste(name, "=", text[1])
  if (append_comma) {
    text[length(text)] <- paste0(text[length(text)], ",")
  }
  text
}

is_trigger_call <- function(expr) {
  tryCatch(
    safe_deparse(expr[[1]]) %in% trigger_fns,
    error = error_false
  )
}
