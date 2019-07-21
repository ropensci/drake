#' @title Define custom columns in a [drake_plan()].
#' @description Not a user-side function. Please use from within
#'   [drake_plan()] only.
#' @export
#' @keywords internal
#' @inheritSection drake_plan Keywords
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

#' @title Declare input files and directories.
#' @description `file_in()` marks individual files
#'   (and whole directories) that your targets depend on.
#' @details As of `drake` 7.4.0, `file_in()` and `file_out()` have
#'   experimental support for URLs. If the file name begins with
#'   "http://", "https://", or "ftp://", [make()] attempts
#'   to check the ETag to see if the data changed from last time.
#'   If no ETag can be found, `drake` simply uses the ETag
#'   from last [make()] and registers the file as unchanged
#'   (which prevents your workflow from breaking if you lose
#'   internet access). If this approach to tracking remote data
#'   does not work for you, consider a custom trigger:
#'   <https://ropenscilabs.github.io/drake-manual/triggers.html>.
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
#' vis_drake_graph(config)
#' })
#' }
file_in <- function(...) {
  as.character(c(...))
}

#' @title Declare output files and directories.
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
#' vis_drake_graph(config)
#' })
#' }
file_out <- file_in

#' @title Declare `knitr`/`rmarkdown` source files
#'   as dependencies.
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
