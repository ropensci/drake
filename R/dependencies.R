#' @title List the dependencies of a function, workflow plan command,
#'   or knitr report source file.
#' @description Intended for debugging and checking your project.
#'   The dependency structure of the components of your analysis
#'   decides which targets are built and when.
#' @details
#'   The `globals` slot of the output list contains candidate globals only.
#'   Each global will be treated as an actual dependency if and only if
#'   it is either a target or an item in the `envir` argument to [make()].
#'
#'   If the argument is a `knitr` report
#'   (for example, `file_store("report.Rmd")` or `"\"report.Rmd\""`)
#'   the the dependencies of the expected compiled
#'   output will be given. For example, `deps_code(file_store("report.Rmd"))`
#'   will return target names found in calls to [loadd()]
#'   and [readd()] in active code chunks.
#'   These [loadd()]/[readd()] targets are needed
#'   in order to run `knit(knitr_in("report.Rmd"))`
#'   to produce the output file `"report.md"`, so technically,
#'   they are dependencies of `"report.md"`, not `"report.Rmd"`.
#'
#'   The [file_store()] function
#'   alerts `drake` utility functions to file names by
#'   enclosing them in literal double quotes.
#'   (For example, `file_store("report.Rmd")` is just `"\"report.Rmd\""`.)
#'
#'   `drake` takes special precautions so that a target/import
#'   does not depend on itself. For example, `deps_code(f)`` might return
#'   `"f"` if `f()` is a recursive function, but [make()] just ignores
#'   this conflict and runs as expected. In other words, [make()]
#'   automatically removes all self-referential loops in the dependency
#'   network.
#' @seealso [deps_target()] [make()] [drake_plan()] [drake_config()]
#' @export
#' @param x a language object (code), character string (code as text),
#'   or imported function to analyze for dependencies.
#' @return Names of dependencies listed by type (object, input file, etc).
#'   The `globals` slot of the output list contains candidate globals only.
#'   Each global will be treated as an actual dependency if and only if
#'   it is either a target or an item in the `envir` argument to [make()].
#' @examples
#' # Your workflow likely depends on functions in your workspace.
#' f <- function(x, y) {
#'   out <- x + y + g(x)
#'   saveRDS(out, "out.rds")
#' }
#' # Find the dependencies of f. These could be R objects/functions
#' # in your workspace or packages. Any file names or target names
#' # will be ignored.
#' deps_code(f)
#' # Define a workflow plan data frame that uses your function f().
#' my_plan <- drake_plan(
#'   x = 1 + some_object,
#'   my_target = x + readRDS(file_in("tracked_input_file.rds")),
#'   return_value = f(x, y, g(z + w)),
#'   strings_in_dots = "literals"
#' )
#' # Get the dependencies of workflow plan commands.
#' # Here, the dependencies could be R functions/objects from your workspace
#' # or packages, imported files, or other targets in the workflow plan.
#' deps_code(my_plan$command[1])
#' deps_code(my_plan$command[2])
#' deps_code(my_plan$command[3])
#' # New: you can also supply language objects.
#' deps_code(expression(x + 123))
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # Dependencies of the knitr-generated targets like 'report.md'
#' # include targets/imports referenced with `readd()` or `loadd()`.
#' deps_code(file_store("report.Rmd"))
#' })
#' }
deps_code <- function(x) {
  if (is.function(x)) {
    import_dependencies(x)
  } else if (is_file(x) && file.exists(drake_unquote(x))) {
    knitr_deps(drake_unquote(x))
  } else if (is.character(x)) {
    command_dependencies(x)
  } else{
    code_dependencies(x)
  }
}

#' @title List the dependencies of one or more targets
#' @description Intended for debugging and checking your project.
#'   The dependency structure of the components of your analysis
#'   decides which targets are built and when.
#' @seealso [deps_code()]
#' @export
#' @param target a symbol denoting a target name, or if `character_only`
#'   is TRUE, a character scalar denoting a target name.
#' @param config an output list from [drake_config()]
#' @param character_only logical, whether to assume target is a character
#'   string rather than a symbol.
#' @return Names of dependencies listed by type (object, input file, etc).
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- drake_config(my_plan)
#' deps_target("regression1_small", config = config)
#' })
#' }
deps_target <- function(
  target,
  config = read_drake_config(),
  character_only = FALSE
) {
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  config$layout[[target]]$deps_build
}

#' @title Find out why a target is out of date.
#' @description The dependency profile can give you
#'   a hint as to why a target is out of date.
#'   It can tell you if
#'   - at least one input file changed,
#'   - at least one output file changed,
#'   - or a non-file dependency changed. For this last part,
#'     the imports need to be up to date in the cache,
#'     which you can do with `outdated()` or
#'     `make(skip_targets = TRUE)`.
#'   Unfortunately, `dependency_profile()` does not
#'   currently get more specific than that.
#' @return A data frame of the old hashes and
#'   new hashes of the data frame, along with
#'   an indication of which hashes changed since
#'   the last [make()].
#' @export
#' @seealso [diagnose()],
#'   [deps_code()], [make()],
#'   [drake_config()]
#' @param target name of the target
#' @param config configuration list output by
#'   [drake_config()] or [make()]
#' @param character_only logical, whether to assume `target`
#'   is a character string rather than a symbol
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Load drake's canonical example.
#' config <- make(my_plan) # Run the project, build the targets.
#' # Get some example dependency profiles of targets.
#' dependency_profile(small, config = config)
#' # Change a dependency.
#' simulate <- function(x) {}
#' # Update the in-memory imports in the cache
#' # so dependency_profile can detect changes to them.
#' # Changes to targets are already cached.
#' make(my_plan, skip_targets = TRUE)
#' # The dependency hash changed.
#' dependency_profile(small, config = config)
#' })
#' }
dependency_profile <- function(
  target,
  config = drake::read_drake_config(),
  character_only = FALSE
) {
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  if (!config$cache$exists(key = target, namespace = "meta")) {
    stop("no recorded metadata for target ", target, ".")
  }
  meta <- config$cache$get(key = target, namespace = "meta")
  if (!length(meta$command)) {
    meta$command <- NA
  }
  old_hashes <- meta[c(
    "command",
    "dependency_hash",
    "input_file_hash",
    "output_file_hash"
  )]
  old_hashes <- unlist(old_hashes)
  old_hashes <- unname(old_hashes)
  old_hashes[1] <- digest::digest(
    paste(old_hashes[1], collapse = ""),
    algo = config$long_hash_algo,
    serialize = FALSE
  )
  layout <- config$layout[[target]]
  new_hashes <- c(
    digest::digest(
      paste(layout$command_standardized, collapse = ""),
      algo = config$long_hash_algo,
      serialize = FALSE
    ),
    dependency_hash(target, config),
    input_file_hash(target, config),
    output_file_hash(target, config)
  )
  weak_tibble(
    hash = c("command", "depend", "file_in", "file_out"),
    changed = old_hashes != new_hashes,
    old_hash = old_hashes,
    new_hash = new_hashes
  )
}

#' @title List the targets and imports
#'   that are reproducibly tracked.
#' @description In other words, list all the layout
#' in your project's dependency network.
#' @export
#' @return A character vector with the names of reproducibly-tracked targets.
#' @param config An output list from [drake_config()].
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Load the canonical example for drake.
#' # List all the targets/imports that are reproducibly tracked.
#' config <- drake_config(my_plan)
#' tracked(config)
#' })
#' }
tracked <- function(config) {
  out <- lightly_parallelize(
    X = V(config$graph)$name,
    FUN = function(target) {
      out <- config$layout[[target]]$deps_build
      out <- as.list(out)
      out <- unlist(out)
      c(out, target)
    },
    jobs = config$jobs
  )
  clean_dependency_list(out)
}

dependencies <- function(targets, config, reverse = FALSE) {
  if (!length(targets)) {
    return(character(0))
  }
  opt <- igraph::igraph_opt("return.vs.es")
  on.exit(igraph::igraph_options(return.vs.es = opt))
  igraph::igraph_options(return.vs.es = FALSE)
  index <- adjacent_vertices(
    graph = config$graph,
    v = targets,
    mode = ifelse(reverse, "out", "in")
  )
  index <- unlist(index)
  index <- unique(index)
  igraph::V(config$graph)$name[index + 1]
}

nonfile_target_dependencies <- function(targets, config, jobs = 1) {
  deps <- dependencies(targets = targets, config = config)
  out <- parallel_filter(x = deps, f = is_not_file, jobs = jobs)
  intersect(out, config$plan$target)
}

import_dependencies <- function(expr, exclude = character(0), globals = NULL) {
  deps <- code_dependencies(expr, exclude = exclude, globals = globals)
  # Imported functions can't have file_out() deps # nolint
  # or target dependencies from knitr code chunks.
  # However, file_in()s are totally fine. # nolint
  deps$file_out <- deps$strings <- NULL
  deps
}

command_dependencies <- function(
  command,
  exclude = character(0),
  globals = NULL
) {
  if (!length(command)) {
    return()
  }
  expr <- command
  if (is.character(command)){
    expr <- parse(text = command, keep.source = FALSE)
  }
  deps <- code_dependencies(
    expr,
    exclude = exclude,
    globals = globals
  )
  deps$strings <- NULL

  # TODO: this block can go away when `drake`
  # stops supporting single-quoted file names.

  use_new_file_api <- identical(
    pkgconfig::get_config("drake::strings_in_dots"),
    "literals"
  )
  if (use_new_file_api) {
    files <- character(0)
  } else {
    if (!is.character(command)){
      command <- wide_deparse(command)
    }
    files <- extract_filenames(command)
  }
  if (length(files)) {
    files <- drake_unquote(files)
    files <- drake_quotes(files, single = FALSE)
    warn_single_quoted_files(files = files, deps = deps)
    files <- setdiff(files, deps$file_out)
    deps$file_in <- base::union(deps$file_in, files)
  }

  # TODO: remove this bit when we're confident
  # users have totally switched to `knitr_in()`.
  # Turn it off right away if users elect for the new file API.
  # I know strings_in_dots is not really meant to do this,
  # but pkgconfig is only a temporary solution to manage
  # the deprecation anyway.
  if (!use_new_file_api) {
    deps$loadd <- base::union(
      deps$loadd, knitr_deps(find_knitr_doc(command))
    )
    deps$loadd <- unique(deps$loadd)
  }

  # This bit stays the same.
  select_nonempty(deps)
}

# TODO: this function can go away when drake
# stops supporting single-quoted file names
warn_single_quoted_files <- function(files, deps) {
  old_api_files <- drake_unquote(files)
  new_api_files <- c(deps$file_in, deps$file_out, deps$knitr_in)
  new_api_files <- drake_unquote(new_api_files)
  warn_files <- setdiff(old_api_files, new_api_files)
  if (!length(warn_files)) {
    return()
  }
  warning(
    "Files in a command declared with single-quotes:\n",
    multiline_message(warn_files),
    "\nThe use of single-quotes to declare files is deprecated. ",
    "Use file_in(), file_out(), and knitr_in() ",
    "in your commands. See `?drake_plan` for examples.",
    call. = FALSE
  )
}
