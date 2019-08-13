#' @title List the dependencies of a function or command
#' \lifecycle{stable}
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
    out <- cdl_import_dependencies(x)
  } else {
    if (is.character(x)) {
      x <- parse(text = x)
    }
    out <- cdl_command_dependencies(x)
  }
  display_deps_list(decode_deps_list(out))
}

#' @title List the dependencies of a target
#' \lifecycle{stable}
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
#' isolate_example("Quarantine side effects.", {
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
  log_msg("begin deps_target()", target = target, config = config)
  on.exit(
    log_msg("end deps_target()", target = target, config = config),
    add = TRUE
  )
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  out <- config$layout[[target]]$deps_build
  out <- decode_deps_list(out)
  display_deps_list(select_nonempty(out))
}

#' @title Find the drake dependencies of a dynamic knitr report target.
#' \lifecycle{stable}
#' @export
#' @seealso [deps_code()], [deps_target()]
#' @description Dependencies in `knitr` reports are marked
#'   by [loadd()] and [readd()] in active code chunks.
#' @return A data frame of dependencies.
#' @param path Encoded file path to the `knitr`/R Markdown document.
#'   Wrap paths in [file_store()] to encode.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' deps_knitr("report.Rmd")
#' })
#' }
deps_knitr <- function(path) {
  display_deps_list(decode_deps_list(get_deps_knitr(path)))
}

get_deps_knitr <- function(target) {
  if (!length(target)) {
    return(list())
  }
  out <- new_code_analysis_results()
  if (is_encoded_path(target)) {
    target <- decode_path(target)
  }
  analyze_knitr_file(target, out)
  list_code_analysis_results(out)
}

decode_deps_list <- function(x) {
  for (field in c("file_in", "file_out", "knitr_in")) {
    if (length(x[[field]])) {
      x[[field]] <- decode_path(x[[field]])
    }
  }
  if (length(x$namespaced)) {
    x$namespaced <- decode_namespaced(x$namespaced)
  }
  x
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
#' \lifecycle{stable}
#' @description The dependency profile can give you
#'   a hint as to why a target is out of date.
#'   It can tell you if
#'   - the command changed
#'     ([deps_profile()] reports the *hash* of the command,
#'     not the command itself)
#'   - at least one input file changed,
#'   - at least one output file changed,
#'   - or a non-file dependency changed. For this last part,
#'     the imports need to be up to date in the cache,
#'     which you can do with `outdated()` or
#'     `make(skip_targets = TRUE)`.
#'   - the pseudo-random number generator seed changed.
#'   Unfortunately, `deps_profile()` does not
#'   currently get more specific than that.
#' @return A data frame of old and new values for each
#'   of the main triggers, along with
#'   an indication of which values changed since
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
#' isolate_example("Quarantine side effects.", {
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
  old_values <- meta[c(
    "command",
    "dependency_hash",
    "input_file_hash",
    "output_file_hash",
    "seed"
  )]
  old_values <- unlist(old_values)
  old_values <- unname(old_values)
  old_values[1] <- digest::digest(
    paste(old_values[1], collapse = ""),
    algo = config$cache$hash_algorithm,
    serialize = FALSE
  )
  layout <- config$layout[[target]]
  new_values <- c(
    digest::digest(
      paste(layout$command_standardized, collapse = ""),
      algo = config$cache$hash_algorithm,
      serialize = FALSE
    ),
    dependency_hash(target, config),
    input_file_hash(target, config),
    output_file_hash(target, config),
    as.integer(
      layout$seed %||NA% seed_from_basic_types(config$seed, target)
    )
  )
  weak_tibble(
    name = c("command", "depend", "file_in", "file_out", "seed"),
    changed = old_values != new_values,
    old = old_values,
    new = new_values
  )
}

#' @title List the targets and imports that are reproducibly tracked.
#' \lifecycle{stable}
#' @description List all the layout
#' in your project's dependency network.
#' @export
#' @return A character vector with the names of reproducibly-tracked targets.
#' @param config An output list from [drake_config()].
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
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
    jobs = config$jobs_preprocess
  )
  display_keys(clean_dependency_list(out), config)
}

clean_dependency_list <- function(x) {
  sort(clean_nested_char_list(x))
}

clean_nested_char_list <- function(x) {
  if (!length(x)){
    return(character(0))
  }
  x <- unlist(x)
  x <- unname(x)
  x <- as.character(x)
  x <- unique(x)
}
