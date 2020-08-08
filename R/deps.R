#' @title List the dependencies of a function or command
#' `r lifecycle::badge("stable")`
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
    out <- cds_import_dependencies(x)
  } else {
    if (is.character(x)) {
      x <- parse(text = x)
    }
    out <- cds_command_dependencies(x)
  }
  display_deps_list(decode_deps_list(out))
}

#' @title List the dependencies of a target
#' `r lifecycle::badge("stable")`
#' @description Intended for debugging and checking your project.
#'   The dependency structure of the components of your analysis
#'   decides which targets are built and when.
#' @seealso [deps_code()], [deps_knitr()]
#' @export
#' @param target A symbol denoting a target name, or if `character_only`
#'   is TRUE, a character scalar denoting a target name.
#' @param ... Arguments to [make()], such as `plan` and `targets`.
#' @param character_only Logical, whether to assume target is a character
#'   string rather than a symbol.
#' @param config Deprecated.
#' @return A data frame with the dependencies listed by type
#'   (globals, files, etc).
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' deps_target(regression1_small, my_plan)
#' })
#' }
deps_target <- function(
  target,
  ...,
  character_only = FALSE,
  config = NULL
) {
}

#' @title Internal function with a drake_config() argument
#' @export
#' @keywords internal
#' @description Not a user-side function.
#' @param target Name of a target.
#' @param character_only Logical, whether to interpret
#'   `target` as a character (`TRUE`) or a symbol (`FALSE`).
#' @param config A [drake_config()] object.
deps_target_impl <- function(
  target,
  config,
  character_only = FALSE
) {
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  out <- config$spec[[target]]$deps_build
  out <- decode_deps_list(out)
  out <- display_deps_list(out)
  out$hash <- config$cache$mget_hash(out$name)
  out
}

body(deps_target) <- config_util_body(deps_target_impl)

#' @title Find the drake dependencies of a dynamic knitr report target.
#' `r lifecycle::badge("stable")`
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
  out <- new_drake_deps_ht()
  if (is_encoded_path(target)) {
    target <- redecode_path(target)
  }
  analyze_knitr_file(target, out, restrict = NULL)
  out <- lapply(out, ht_list)
  do.call(new_drake_deps, out)
}

decode_deps_list <- function(x) {
  for (field in c("file_in", "file_out", "knitr_in")) {
    if (length(x[[field]])) {
      x[[field]] <- redecode_path(x[[field]])
    }
  }
  if (length(x$namespaced)) {
    x$namespaced <- redecode_namespaced(x$namespaced)
  }
  x
}

display_deps_list <- function(x) {
  if (!length(x)) {
    return(weak_tibble(name = character(0), type = character(0)))
  }
  x$memory <- NULL
  x <- select_nonempty(x)
  out <- lapply(names(x), function(n) {
    weak_tibble(name = x[[n]], type = n)
  })
  do.call(rbind, out) %|||%
    weak_tibble(name = character(0), type = character(0))
}

#' @title Find out why a target is out of date.
#' `r lifecycle::badge("stable")`
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
#' @param ... Arguments to [make()], such as `plan` and `targets`.
#' @param config Deprecated.
#' @param character_only Logical, whether to assume `target`
#'   is a character string rather than a symbol.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Load drake's canonical example.
#' make(my_plan) # Run the project, build the targets.
#' # Get some example dependency profiles of targets.
#' deps_profile(small, my_plan)
#' # Change a dependency.
#' simulate <- function(x) {}
#' # Update the in-memory imports in the cache
#' # so deps_profile can detect changes to them.
#' # Changes to targets are already cached.
#' make(my_plan, skip_targets = TRUE)
#' # The dependency hash changed.
#' deps_profile(small, my_plan)
#' }
#' })
#' }
deps_profile <- function(
  target,
  ...,
  character_only = FALSE,
  config = NULL
) {
}

#' @title Internal function with a drake_config() argument
#' @export
#' @keywords internal
#' @description Not a user-side function.
#' @param target Name of a target.
#' @param character_only Logical, whether to interpret
#'   `target` as a character (`TRUE`) or a symbol (`FALSE`).
#' @param config A [drake_config()] object.
deps_profile_impl <- function(
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
  if (!length(meta$seed)) {
    meta$seed <- NA_integer_
  }
  fields <- c(
    "command",
    "dependency_hash",
    "input_file_hash",
    "output_file_hash",
    "seed"
  )
  old_values <- lapply(
    fields,
    function(field) {
      meta[[field]] %||% NA
    }
  )
  names(old_values) <- fields
  old_values <- unlist(old_values)
  old_values <- unname(old_values)
  elt <- paste(old_values[1], collapse = "")
  old_values[1] <- config$cache$digest(elt, serialize = FALSE)
  spec <- config$spec[[target]]
  new_values <- c(
    config$cache$digest(
      paste(spec$command_standardized, collapse = ""),
      serialize = FALSE
    ) %||% NA,
    static_dependency_hash(target, config) %||% NA,
    input_file_hash(target, config) %||% NA,
    output_file_hash(target, config) %||% NA,
    resolve_target_seed(target, config) %||% NA
  )
  out <- weak_tibble(
    name = c("command", "depend", "file_in", "file_out", "seed"),
    changed = old_values != new_values,
    old = old_values,
    new = new_values
  )
  if (identical(spec$imported, TRUE)) {
    out <- out[out$name %in% c("depend", "file_in"), ]
  }
  out
}

body(deps_profile) <- config_util_body(deps_profile_impl)

#' @title List the targets and imports that are reproducibly tracked.
#' `r lifecycle::badge("stable")`
#' @description List all the spec
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
      out <- config$spec[[target]]$deps_build
      out <- as.list(out)
      out <- unlist(out)
      c(out, target)
    },
    jobs = config$settings$jobs_preprocess
  )
  config$cache$display_keys(clean_dependency_list(out))
}

clean_dependency_list <- function(x) {
  sort(clean_nested_char_list(x))
}

clean_nested_char_list <- function(x) {
  x <- unlist(x)
  x <- unname(x)
  x <- as.character(x)
  x <- unique(x)
}
