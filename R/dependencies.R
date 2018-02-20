#' @title List the dependencies of a function, workflow plan command,
#'   or knitr report source file.
#' @description Intended for debugging and checking your project.
#'   The dependency structure of the components of your analysis
#'   decides which targets are built and when.
#' @details If the argument is a single-quoted string that points to
#'   a dynamic knitr report, the dependencies of the expected compiled
#'   output will be given. For example, `deps("'report.Rmd'")`
#'   will return target names found in calls to [loadd()]
#'   and [readd()] in active code chunks.
#'   These targets are needed in order to run `knit('report.Rmd')`
#'   to produce the output file `'report.md'`, so technically,
#'   they are dependencies of `'report.md'`, not `'report.Rmd'`.
#'
#'   `Drake` takes special precautions so that a target/import
#'   does not depend on itself. For example, `deps(f)`` might return
#'   `"f"` if `f()` is a recursive function, but [make()] just ignores
#'   this conflict and runs as expected. In other words, [make()]
#'   automatically removes all self-referential loops in the dependency
#'   network.
#' @export
#' @param x Either a function or a string.
#'   Strings are commands from your workflow plan data frame.
#' @return A character vector, names of dependencies.
#'   Files wrapped in single quotes.
#'   The other names listed are functions or generic R objects.
#' @examples
#' # Your workflow likely depends on functions in your workspace.
#' f <- function(x, y){
#'   out <- x + y + g(x)
#'   saveRDS(out, 'out.rds')
#' }
#' # Find the dependencies of f. These could be R objects/functions
#' # in your workspace or packages. Any file names or target names
#' # will be ignored.
#' deps(f)
#' # Define a workflow plan data frame that uses your function f().
#' my_plan <- drake_plan(
#'   x = 1 + some_object,
#'   my_target = x + readRDS('tracked_input_file.rds'),
#'   return_value = f(x, y, g(z + w))
#' )
#' # Get the dependencies of workflow plan commands.
#' # Here, the dependencies could be R functions/objects from your workspace
#' # or packages, imported files, or other targets in the workflow plan.
#' deps(my_plan$command[1])
#' deps(my_plan$command[2])
#' deps(my_plan$command[3])
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' # Dependencies of the knitr-generated targets like 'report.md'
#' # include targets/imports referenced with `readd()` or `loadd()`.
#' deps("\"report.Rmd\"")
#' })
#' }
deps <- function(x){
  if (is.function(x)){
    out <- import_dependencies(x)
  } else if (is_file(x) && file.exists(drake_unquote(x))){
    out <- knitr_deps(drake_unquote(x))
  } else if (is.character(x)){
    out <- command_dependencies(x)
  } else{
    stop("x must be a character scalar or function.")
  }
  clean_dependency_list(out)
}

#' @title Return the detailed dependency profile
#'   of the target.
#' @description Useful for debugging.
#' For up to date targets, like elements
#' of the returned list should agree: for example,
#' `cached_dependency_hash` and
#' `current_dependency_hash`.
#' @return A list of information that drake takes into account
#'   when examining the dependencies of the target.
#' @export
#' @seealso [read_drake_meta()],
#'   [deps()], [make()],
#'   [config()]
#' @param target name of the target
#' @param config configuration list output by
#'   [config()] or [make()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load drake's canonical exmaple.
#' con <- make(my_plan) # Run the project, build the targets.
#' # Get some example dependency profiles of targets.
#' dependency_profile("small", config = con)
#' dependency_profile("\"report.md\"", config = con)
#' })
#' }
dependency_profile <- function(target, config){
  if (!config$cache$exists(key = target, namespace = "meta")){
    stop("no recorded metadata for target ", target, ".")
  }
  config$plan[["trigger"]] <- NULL
  meta <- config$cache$get(key = target, namespace = "meta")
  deps <- dependencies(target, config)
  hashes_of_dependencies <- self_hash(target = deps, config = config)
  current_dependency_hash <- digest::digest(hashes_of_dependencies,
                                            algo = config$long_hash_algo)
  names(hashes_of_dependencies) <- deps
  out <- list(
    cached_command = meta$command,
    current_command = get_standardized_command(
      target = target, config = config
    ),
    cached_file_modification_time = meta$mtime,
    current_file_modification_time = suppressWarnings(
      file.mtime(drake::drake_unquote(target))
    ),
    cached_file_hash = meta$file,
    current_file_hash = file_hash(target = target, config = config),
    cached_dependency_hash = meta$depends,
    current_dependency_hash = current_dependency_hash,
    hashes_of_dependencies = hashes_of_dependencies
  )
  out[!is.na(out)]
}

#' @title List the targets and imports
#'   that are reproducibly tracked.
#' @description In other words, list all the nodes
#' in your project's dependency network.
#' @export
#' @return A character vector with the names of reproducibly-tracked targets.
#' @param plan workflow plan data frame, same as for function
#'   [make()].
#' @param targets names of targets to build, same as for function
#'   [make()].
#' @param envir environment to import from, same as for function
#'   [make()].
#' @param jobs number of jobs to accelerate the construction
#'   of the dependency graph. A light `mclapply()`-based
#'   parallelism is used if your operating system is not Windows.
#' @param verbose logical, whether to print
#'   progress messages to the console.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load the canonical example for drake.
#' # List all the targets/imports that are reproducibly tracked.
#' tracked(my_plan)
#' })
#' }
tracked <- function(
  plan = read_drake_plan(),
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  jobs = 1,
  verbose = TRUE
){
  force(envir)
  graph <- build_drake_graph(
    plan = plan, targets = targets, envir = envir,
    jobs = jobs, verbose = verbose
  )
  V(graph)$name
}

dependencies <- function(targets, config, reverse = FALSE){
  adjacent_vertices(
    graph = config$graph,
    v = targets,
    mode = ifelse(reverse, "out", "in")
  ) %>%
    lapply(FUN = names) %>%
    clean_dependency_list()
}

nonfile_target_dependencies <- function(targets, config){
  deps <- dependencies(targets = targets, config = config)
  out <- parallel_filter(x = deps, f = is_not_file, jobs = config$jobs)
  intersect(out, config$plan$target)
}

import_dependencies <- function(expr){
  deps <- code_dependencies(expr)
  # Imported functions can't have file_output() deps # nolint
  # or target dependencies from knitr code chunks.
  # However, file_input()s are totally fine. # nolint
  deps$file_output <- deps$loadd <- deps$readd <- NULL
  deps
}

command_dependencies <- function(command){
  if (!length(command)){
    return()
  }
  if (is.na(command)){
    return()
  }
  # TODO: May need to think about changing this bit
  # if we support expressions in workflow plans.
  command <- as.character(command)
  deps <- code_dependencies(parse(text = command))

  # TODO: this block can go away when `drake`
  # stops supporting single-quoted file names.
  files <- extract_filenames(command)
  if (length(files)){
    files <- drake_unquote(files) %>%
      drake_quotes(single = FALSE)
    warn_single_quoted_files(files = files, deps = deps)
    files <- setdiff(files, deps$file_output)
    deps$file_input <- base::union(deps$file_input, files)
  }
  deps$loadd <- base::union(
    deps$loadd, knitr_deps(find_knitr_doc(command))
  ) %>%
    unique
  deps[lapply(deps, length) > 0]
}

# TODO: this function can go away when drake
# stops supporting single-quoted file names
warn_single_quoted_files <- function(files, deps){
  old_api_files <- drake_unquote(files)
  new_api_files <- c(deps$file_input, deps$file_output, deps$knitr_input) %>%
    drake_unquote
  warn_files <- setdiff(old_api_files, new_api_files)
  if (!length(warn_files)){
    return()
  }
  warning(
    "Files in a command declared with single-quotes:\n",
    multiline_message(warn_files),
    "\nThe way to declare files in drake is deprecated. ",
    "Use file_input(), file_output(), and knitr_input() ",
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

code_dependencies <- function(expr){
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
    } else if (is.name(expr) || is.atomic(expr) || is.primitive(expr)) {
      new_globals <- setdiff(
        x = wide_deparse(expr), y = drake_fn_patterns)
      results$globals <<- c(results$globals, new_globals)
    } else if (is.call(expr) || is.recursive(expr)) {
      new_results <- list()
      if (is_loadd_call(expr)){
        new_results <- analyze_loadd(expr)
      } else if (is_readd_call(expr)){
        new_results <- analyze_readd(expr)
      } else if (is_knitr_input_call(expr)){
        new_results <- analyze_knitr_input(expr)
      } else if (is_file_input_call(expr)){
        new_results <- analyze_file_input(expr)
      } else if (is_file_output_call(expr)){
        new_results <- analyze_file_output(expr)
      } else {
        if (wide_deparse(expr[[1]]) %in% c("::", ":::")){
          results$namespaced <<- base::union(
            results$namespaced,
            setdiff(wide_deparse(expr), drake_fn_patterns)
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
  results[lapply(results, length) > 0]
}

find_globals <- function(expr){
  if (is.function(expr)){
    expr <- unwrap_function(expr)
    formals <- names(formals(expr))
    expr <- body(expr)
  } else {
    formals <- character(0)
  }
  # Warning: In collector$results(reset = reset) :
  #  partial argument match of 'reset' to 'resetState'
  suppressWarnings(inputs <- CodeDepends::getInputs(expr))
  base::union(
    inputs@inputs,
    names(inputs@functions)
  ) %>%
    setdiff(y = c(formals, drake_fn_patterns)) %>%
    Filter(f = is_parsable)
}

analyze_loadd <- function(expr){
  expr <- match.call(drake::loadd, as.call(expr))
  args <- parse_loadd_arg_list(expr)
  out <- c(unnamed_in_list(args), args[["list"]])
  list(loadd = setdiff(out, drake_fn_patterns))
}

analyze_readd <- function(expr){
  expr <- match.call(drake::readd, as.call(expr))
  args <- parse_loadd_arg_list(expr)
  list(readd = setdiff(args[["target"]], drake_fn_patterns))
}

analyze_file_input <- function(expr){
  inputs <- CodeDepends::getInputs(expr)
  deps <- drake_quotes(c(inputs@strings, inputs@files), single = FALSE)
  list(file_input = deps)
}

analyze_file_output <- function(expr){
  inputs <- CodeDepends::getInputs(expr)
  deps <- drake_quotes(c(inputs@strings, inputs@files), single = FALSE)
  list(file_output = deps)
}

analyze_knitr_input <- function(expr){
  inputs <- CodeDepends::getInputs(expr)
  files <- c(inputs@strings, inputs@files)
  out <- lapply(files, knitr_deps_list) %>%
    Reduce(f = merge_lists)
  files <- drake_quotes(files, single = FALSE)
  out$knitr_input <- base::union(out$knitr_input, files)
  out
}

parse_loadd_arg_list <- function(expr){
  lapply(as.list(expr)[-1], function(arg){
    inputs <- CodeDepends::getInputs(arg)
    c(inputs@strings, inputs@inputs)
  })
}

unnamed_in_list <- function(x){
  if (!length(names(x))){
    out <- x
  } else {
    out <- x[!nchar(names(x))]
  }
  unlist(out)
}

drake_prefix <- c("", "drake::", "drake:::")
knitr_input_fns <- paste0(drake_prefix, "knitr_input")
file_input_fns <- paste0(drake_prefix, "file_input")
file_output_fns <- paste0(drake_prefix, "file_output")
loadd_fns <- paste0(drake_prefix, "loadd")
readd_fns <- paste0(drake_prefix, "readd")
drake_fn_patterns <- c(
  knitr_input_fns,
  file_input_fns,
  file_output_fns,
  loadd_fns,
  readd_fns
)

is_knitr_input_call <- function(expr){
  wide_deparse(expr[[1]]) %in% c(knitr_input_fns)
}

is_file_input_call <- function(expr){
  wide_deparse(expr[[1]]) %in% file_input_fns
}

is_file_output_call <- function(expr){
  wide_deparse(expr[[1]]) %in% file_output_fns
}

is_loadd_call <- function(expr){
  wide_deparse(expr[[1]]) %in% loadd_fns
}

is_readd_call <- function(expr){
  wide_deparse(expr[[1]]) %in% readd_fns
}
