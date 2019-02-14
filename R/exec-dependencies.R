#' @title List the dependencies of a function or command
#' @description Functions are assumed to be imported,
#'   and language/text are assumed to be commands in a plan.
#' @seealso [deps_target()], [deps_knitr()]
#' @export
#' @param x A function, expression, or text.
#' @return A data frame of the dependencies.
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
#'   return_value = f(x, y, g(z + w))
#' )
#' # Get the dependencies of workflow plan commands.
#' # Here, the dependencies could be R functions/objects from your workspace
#' # or packages, imported files, or other targets in the workflow plan.
#' deps_code(my_plan$command[[1]])
#' deps_code(my_plan$command[[2]])
#' deps_code(my_plan$command[[3]])
#' # You can also supply expressions or text.
#' deps_code(quote(x + y + 123))
#' deps_code("x + y + 123")
deps_code <- function(x) {
  if (is.function(x)) {
    out <- import_dependencies(x)
  } else {
    if (is.character(x)) {
      x <- parse(text = x)
    }
    out <- command_dependencies(x)
  }
  display_deps_list(decode_deps_list(out))
}

#' @title List the dependencies of a target
#' @description Intended for debugging and checking your project.
#'   The dependency structure of the components of your analysis
#'   decides which targets are built and when.
#' @seealso [deps_code()], [deps_knitr()]
#' @export
#' @param target A symbol denoting a target name, or if `character_only`
#'   is TRUE, a character scalar denoting a target name.
#' @param config An output list from [drake_config()].
#' @param character_only Logical, whether to assume target is a character
#'   string rather than a symbol.
#' @return A data frame with the dependencies listed by type
#'   (globals, files, etc).
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
  config,
  character_only = FALSE
) {
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  out <- config$layout[[target]]$deps_build
  out <- decode_deps_list(out)
  display_deps_list(select_nonempty(out))
}

display_deps_list <- function(x) {
  if (!length(x)) {
    return(weak_tibble(name = character(0), type = character(0)))
  }
  x$memory <- NULL
  out <- lapply(names(x), function(n) {
    weak_tibble(name = x[[n]], type = n)
  })
  do.call(rbind, out)
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
#'   Unfortunately, `deps_profile()` does not
#'   currently get more specific than that.
#' @return A data frame of the old hashes and
#'   new hashes of the data frame, along with
#'   an indication of which hashes changed since
#'   the last [make()].
#' @export
#' @seealso [diagnose()],
#'   [deps_code()], [make()],
#'   [drake_config()]
#' @param target Name of the target.
#' @param config Configuration list output by
#'   [drake_config()] or [make()].
#' @param character_only Logical, whether to assume `target`
#'   is a character string rather than a symbol.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build the targets.
#' config <- drake_config(my_plan)
#' # Get some example dependency profiles of targets.
#' deps_profile(small, config = config)
#' # Change a dependency.
#' simulate <- function(x) {}
#' # Update the in-memory imports in the cache
#' # so deps_profile can detect changes to them.
#' # Changes to targets are already cached.
#' make(my_plan, skip_targets = TRUE)
#' # The dependency hash changed.
#' deps_profile(small, config = config)
#' }
#' })
#' }
deps_profile <- function(
  target,
  config,
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
    meta$command <- NA_character_
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
    algo = config$cache$driver$hash_algorithm,
    serialize = FALSE
  )
  layout <- config$layout[[target]]
  new_hashes <- c(
    digest::digest(
      paste(layout$command_standardized, collapse = ""),
      algo = config$cache$driver$hash_algorithm,
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

#' @title List the targets and imports that are reproducibly tracked.
#' @description List all the layout
#' in your project's dependency network.
#' @export
#' @return A character vector with the names of reproducibly-tracked targets.
#' @param config An output list from [drake_config()].
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Load the canonical example for drake.
#' # List all the targets/imports that are reproducibly tracked.
#' config <- drake_config(my_plan)
#' tracked(config)
#' }
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
  display_keys(clean_dependency_list(out), config)
}

import_dependencies <- function(
  expr, exclude = character(0), allowed_globals = NULL
) {
  deps <- analyze_code(
    expr = expr,
    exclude = exclude,
    allowed_globals = allowed_globals
  )
  deps$file_out <- deps$strings <- NULL
  select_nonempty(deps)
}

command_dependencies <- function(
  command,
  exclude = character(0),
  allowed_globals = NULL
) {
  if (!length(command)) {
    return()
  }
  deps <- analyze_code(
    command,
    exclude = exclude,
    allowed_globals = allowed_globals
  )
  deps$strings <- NULL
  select_nonempty(deps)
}
