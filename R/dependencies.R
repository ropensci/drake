#' @title List the dependencies of a function, workflow plan command,
#'   or knitr report source file.
#' @description Intended for debugging and checking your project.
#'   The dependency structure of the components of your analysis
#'   decides which targets are built and when.
#' @details If the argument is a `knitr` report
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
#'   `Drake` takes special precautions so that a target/import
#'   does not depend on itself. For example, `deps_code(f)`` might return
#'   `"f"` if `f()` is a recursive function, but [make()] just ignores
#'   this conflict and runs as expected. In other words, [make()]
#'   automatically removes all self-referential loops in the dependency
#'   network.
#' @seealso deps_targets deps_files make drake_plan drake_config
#' @export
#' @param x a language object (code), character string (code as text),
#'   or imported function to analyze for dependencies.
#' @return Names of dependencies listed by type (object, input file, etc).
#' @examples
#' # Your workflow likely depends on functions in your workspace.
#' f <- function(x, y){
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
deps_code <- function(x){
  if (is.function(x)){
    import_dependencies(x)
  } else if (is_file(x) && file.exists(drake_unquote(x))){
    knitr_deps(drake_unquote(x))
  } else if (is.character(x)){
    command_dependencies(x)
  } else{
    code_dependencies(x)
  }
}

#' @title List the dependencies of one or more targets
#' @description Unlike [deps_code()], `deps_targets()` just lists
#'   the jobs that lie upstream of the `targets` on the workflow
#'   dependency graph, and `file_out()` files are not included.
#' @export
#' @param targets a character vector of target names
#' @param config an output list from [drake_config()]
#' @param reverse logical, whether to compute reverse dependencies
#'   (targets immediately downstream) instead of ordinary dependencies.
#' @return A character vector, names of dependencies.
#'   Files wrapped in escaped double quotes.
#'   The other names listed are functions or generic R objects.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- drake_config(my_plan)
#' deps_targets("regression1_small", config = config)
#' deps_targets(c("small", "large"), config = config, reverse = TRUE)
#' })
#' }
deps_targets <- function(
  targets,
  config = read_drake_config(),
  reverse = FALSE
){
  dependencies(targets = targets, config = config, reverse = reverse)
}

#' @title Find out why a target is out of date.
#' @description The dependency profile can tell you
#'   why a target is out of date. It tells you the specific
#'   dependencies that changed since the last [make()],
#'   as well as whether any of the built-in [trigger()]s
#'   (the command, depend, and file triggers) will be activated.
#' @return A data frame of the old hashes and
#'   new hashes of each dependency and trigger, along with
#'   a logical column to indicate which hashes changed since
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
#' simulate <- function(x){}
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
){
  if (!character_only){
    target <- as.character(substitute(target))
  }
  if (!config$cache$exists(key = target, namespace = "meta")){
    stop("no recorded metadata for target ", target, ".")
  }
  old_profile <- config$cache$get(
    key = target, namespace = "meta") %>%
    one_profile(config = config)
  new_profile <- list(
    command = get_standardized_command(target, config),
    dependency_hash = dependency_hash(target, config),
    input_file_hash = input_file_hash(target, config),
    output_file_hash = output_file_hash(target, config)
  ) %>%
    one_profile(config = config)
  both_profiles <- merge(
    x = old_profile,
    y = new_profile,
    by = "ind",
    all = TRUE
  )
  changed <- both_profiles$values.x != both_profiles$values.y
  changed[is.na(changed)] <- TRUE
  out <- tibble::tibble(
    name = both_profiles$ind,
    changed = changed,
    old_hash = both_profiles$values.x,
    new_hash = both_profiles$values.y
  )
  dplyr::bind_rows(
    out[grepl("^_", out$name), ],
    out[!grepl("^_", out$name), ]
  )
}

one_profile <- function(meta, config){
  c(
    `_command_trigger` = digest::digest(
      meta$command,
      algo = config$long_hash_algo
    ),
    `_depend_trigger` = digest::digest(
      meta$dependency_hash,
      algo = config$long_hash_algo
    ),
    `_file_trigger` = digest::digest(
      c(meta$input_file_hash, meta$input_file_hash),
      algo = config$long_hash_algo
    ),
    meta$dependency_hash,
    meta$input_file_hash,
    meta$output_file_hash
  ) %>%
    utils::stack()
}

#' @title List the targets and imports
#'   that are reproducibly tracked.
#' @description In other words, list all the nodes
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
tracked <- function(config){
  lightly_parallelize(
    X = V(config$graph)$name,
    FUN = function(target){
      vertex_attr(
        graph = config$graph,
        name = "deps",
        index = target
      )[[1]] %>%
        as.list %>%
        unlist %>%
        c(target)
    },
    jobs = config$jobs
  ) %>%
    clean_dependency_list
}

dependencies <- function(targets, config, reverse = FALSE){
  if (!length(targets)){
    return(character(0))
  }
  opt <- igraph::igraph_opt("return.vs.es")
  on.exit(igraph::igraph_options(return.vs.es = opt))
  igraph::igraph_options(return.vs.es = FALSE)
  index <- adjacent_vertices(
    graph = config$graph,
    v = targets,
    mode = ifelse(reverse, "out", "in")
  ) %>%
    unlist %>%
    unique
  igraph::V(config$graph)$name[index + 1]
}

nonfile_target_dependencies <- function(targets, config, jobs = 1){
  deps <- dependencies(targets = targets, config = config)
  out <- parallel_filter(x = deps, f = is_not_file, jobs = jobs)
  intersect(out, config$plan$target)
}

import_dependencies <- function(expr, exclude = character(0)){
  deps <- code_dependencies(expr, exclude = exclude)
  # Imported functions can't have file_out() deps # nolint
  # or target dependencies from knitr code chunks.
  # However, file_in()s are totally fine. # nolint
  deps$file_out <- deps$strings <- NULL
  deps
}

command_dependencies <- function(command, exclude = character(0)){
  if (!length(command)){
    return()
  }
  command <- as.character(command)
  deps <- code_dependencies(parse(text = command), exclude = exclude)
  deps$strings <- NULL

  # TODO: this block can go away when `drake`
  # stops supporting single-quoted file names.

  use_new_file_api <- identical(
    pkgconfig::get_config("drake::strings_in_dots"),
    "literals"
  )
  if (use_new_file_api){
    files <- character(0)
  } else {
    files <- extract_filenames(command)
  }
  if (length(files)){
    files <- drake_unquote(files) %>%
      drake_quotes(single = FALSE)
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
  if (!use_new_file_api){
    deps$loadd <- base::union(
      deps$loadd, knitr_deps(find_knitr_doc(command))
    ) %>%
      unique
  }

  # This bit stays the same.
  deps[purrr::map_int(deps, length) > 0]
}

# TODO: this function can go away when drake
# stops supporting single-quoted file names
warn_single_quoted_files <- function(files, deps){
  old_api_files <- drake_unquote(files)
  new_api_files <- c(deps$file_in, deps$file_out, deps$knitr_in) %>%
    drake_unquote
  warn_files <- setdiff(old_api_files, new_api_files)
  if (!length(warn_files)){
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

# Walk through function f and find `pkg::fun()` and `pkg:::fun()` calls.
find_namespaced_functions <- function(f, found = character(0)){
  if (is.function(f)){
    return(find_namespaced_functions(body(f), found))
  } else if (is.call(f) && wide_deparse(f[[1]]) %in% c("::", ":::")){
    found <- c(found, wide_deparse(f))
  } else if (is.recursive(f)){
    v <- lapply(as.list(f), find_namespaced_functions, found)
    found <- unique(c(found, unlist(v)))
  }
  found
}

is_vectorized <- function(funct){
  if (!is.function(funct)){
    return(FALSE)
  }
  if (!is.environment(environment(funct))){
    return(FALSE)
  }
  vectorized_names <- "FUN" # Chose not to include other names.
  if (!all(vectorized_names %in% ls(environment(funct)))){
    return(FALSE)
  }
  f <- environment(funct)[["FUN"]]
  is.function(f)
}

unwrap_function <- function(funct){
  if (is_vectorized(funct)) {
    funct <- environment(funct)[["FUN"]]
  }
  funct
}

code_dependencies <- function(expr, exclude = character(0)){
  if (
    !is.function(expr) &&
    !is.expression(expr) &&
    !is.language(expr)
  ){
    return(list())
  }
  results <- list()
  # `walk()` analyzes `drake`-specific calls
  # in an expression or function.
  # It sees `results` in its lexical scope.
  walk <- function(expr){
    if (!length(expr)){
      return()
    } else if (is.function(expr)) {
      expr <- unwrap_function(expr)
      if (typeof(expr) != "closure"){
        expr <- function(){} # nolint: curly braces are necessary
      }
      walk(body(expr))
    } else if (is.name(expr)) {
      new_globals <- setdiff(x = wide_deparse(expr), y = drake_fn_patterns)
      results$globals <<- c(results$globals, new_globals)
    } else if (is.character(expr)) {
      results$strings <<- c(results$strings, expr)
    } else if (is.language(expr) && (is.call(expr) || is.recursive(expr))) {
      new_results <- list()
      if (is_loadd_call(expr)){
        new_results <- analyze_loadd(expr)
      } else if (is_readd_call(expr)){
        new_results <- analyze_readd(expr)
      } else if (is_knitr_in_call(expr)){
        new_results <- analyze_knitr_in(expr)
      } else if (is_file_in_call(expr)){
        new_results <- analyze_file_in(expr)
      } else if (is_file_out_call(expr)){
        new_results <- analyze_file_out(expr)
      } else if (!is_ignore_call(expr)){
        if (wide_deparse(expr[[1]]) %in% c("::", ":::")){
          new_results <- list(
            namespaced = setdiff(wide_deparse(expr), drake_fn_patterns)
          )
        } else {
          lapply(X = expr, FUN = walk)
        }
      }
      results <<- merge_lists(x = results, y = new_results)
    }
  }
  walk(expr)
  results$globals <- intersect(results$globals, find_globals(expr))
  if (length(exclude) > 0){
    results <- lapply(
      X = results,
      FUN = function(x){
        setdiff(x, exclude)
      }
    )
  }
  results[purrr::map_int(results, length) > 0]
}

find_globals <- function(fun){
  if (!is.function(fun)){
    f <- function(){} # nolint
    body(f) <- as.call(append(as.list(body(f)), fun))
    fun <- f
  }
  if (typeof(fun) != "closure"){
    return(character(0))
  }
  fun <- unwrap_function(fun)
  # The tryCatch statement fixes a strange bug in codetools
  # for R 3.3.3. I do not understand it.
  tryCatch(
    codetools::findGlobals(fun = fun, merge = TRUE),
    error = function(e){
      fun <- eval(parse(text = rlang::expr_text(fun))) # nocov
      codetools::findGlobals(fun = fun, merge = TRUE)  # nocov
    }
  ) %>%
    setdiff(y = c(drake_fn_patterns, ".")) %>%
    Filter(f = is_parsable)
}

analyze_loadd <- function(expr){
  expr <- match.call(drake::loadd, as.call(expr))
  expr <- expr[-1]
  unnamed <- code_dependencies(expr[which_unnamed(expr)])
  out <- c(
    unnamed$globals,
    unnamed$strings,
    code_dependencies(expr["list"])$strings
  )
  list(loadd = setdiff(out, drake_fn_patterns))
}

analyze_readd <- function(expr){
  expr <- match.call(drake::readd, as.call(expr))
  deps <- unlist(code_dependencies(expr["target"])[c("globals", "strings")])
  list(readd = setdiff(deps, drake_fn_patterns))
}

analyze_file_in <- function(expr){
  expr <- expr[-1]
  deps <- drake_quotes(code_dependencies(expr)$strings, single = FALSE)
  list(file_in = deps)
}

analyze_file_out <- function(expr){
  expr <- expr[-1]
  deps <- drake_quotes(code_dependencies(expr)$strings, single = FALSE)
  list(file_out = deps)
}

analyze_knitr_in <- function(expr){
  expr <- expr[-1]
  files <- code_dependencies(expr)$strings
  out <- lapply(files, knitr_deps_list) %>%
    Reduce(f = merge_lists)
  files <- drake_quotes(files, single = FALSE)
  out$knitr_in <- base::union(out$knitr_in, files)
  out
}

ignore_ignore <- function(expr){
  if (is.character(expr)){
    expr <- parse(text = expr)
  }
  recurse_ignore(expr)
}

recurse_ignore <- function(x) {
  if (is.function(x) && !is.primitive(x) && !is.null(body(x))){
    body(x) <- recurse_ignore(body(x))
  } else if (is_callish(x)){
    if (is_ignore_call(x)) {
      x <- quote(ignore())
    } else {
      x[] <- purrr::map(as.list(x), recurse_ignore)
    }
  }
  x
}

which_unnamed <- function(x){
  if (!length(names(x))){
    rep(TRUE, length(x))
  } else {
    !nzchar(names(x))
  }
}

is_callish <- function(x){
  length(x) > 0 && is.language(x) && (is.call(x) || is.recursive(x))
}

pair_text <- function(x, y){
  apply(expand.grid(x, y), 1, paste0, collapse = "")
}

drake_prefix <- c("", "drake::", "drake:::")
knitr_in_fns <- pair_text(drake_prefix, c("knitr_in"))
file_in_fns <- pair_text(drake_prefix, c("file_in"))
file_out_fns <- pair_text(drake_prefix, c("file_out"))
loadd_fns <- pair_text(drake_prefix, "loadd")
readd_fns <- pair_text(drake_prefix, "readd")
ignore_fns <- pair_text(drake_prefix, "ignore")
target_fns <- pair_text(drake_prefix, "target")
trigger_fns <- pair_text(drake_prefix, "trigger")
drake_fn_patterns <- c(
  knitr_in_fns,
  file_in_fns,
  file_out_fns,
  loadd_fns,
  readd_fns,
  ignore_fns,
  target_fns,
  trigger_fns
)

is_knitr_in_call <- function(expr){
  wide_deparse(expr[[1]]) %in% c(knitr_in_fns)
}

is_file_in_call <- function(expr){
  wide_deparse(expr[[1]]) %in% file_in_fns
}

is_file_out_call <- function(expr){
  wide_deparse(expr[[1]]) %in% file_out_fns
}

is_loadd_call <- function(expr){
  wide_deparse(expr[[1]]) %in% loadd_fns
}

is_readd_call <- function(expr){
  wide_deparse(expr[[1]]) %in% readd_fns
}

is_ignore_call <- function(expr){
  wide_deparse(expr[[1]]) %in% ignore_fns
}

is_target_call <- function(expr){
  tryCatch(
    wide_deparse(expr[[1]]) %in% target_fns,
    error = error_false
  )
}

is_trigger_call <- function(expr){
  tryCatch(
    wide_deparse(expr[[1]]) %in% trigger_fns,
    error = error_false
  )
}
