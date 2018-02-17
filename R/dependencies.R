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
#' deps("report.Rmd")
#' })
#' }
deps <- function(x){
  if (is.function(x)){
    out <- code_dependencies(x)
  } else if (file.exists(drake_unquote(x))){
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
  plan = drake_plan(),
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
    deps$file_input <- c(deps$file_input, files)
  }
  deps$loadd <- c(deps$loadd, knitr_deps(find_knitr_doc(command))) %>%
    unique
  deps
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

##############################
### NEW FILE DETECTION API ###
### AND CODE ANALYSIS      ###
##############################

#' @title Declare the file inputs of a workflow plan command.
#' @description Use this function to help write the commands
#'   in your workflow plan data frame. Treat the output as a character vector.
#' @export
#' @seealso file_output knitr_input
#' @return A character vector of declared files.
#' @param ... Symbols or character vectors denoting file inputs.
#' @export
#' @examples
#' \dontrun{
#' test_with_dir("Contain side effects", {
#' # The `file_output()` and `file_input()` functions
#' # just takes in strings and returns them.
#' file_output("summaries.txt")
#' # Their main purpose is to orchestrate your custom files
#' # in your workflow plan data frame.
#' suppressWarnings(
#'   plan <- drake_plan(
#'     write.csv(mtcars, file_output("mtcars.csv")),
#'     contents = read.csv(file_input("mtcars.csv")),
#'     strings_in_dots = "literals" # deprecated but useful: no single quotes needed. # nolint
#'   )
#' )
#' plan
#' # Drake knows "\"mtcars.csv\"" is the first target
#' # and a dependency of `contents`. See for yourself:
#' config <- make(plan)
#' file.exists("mtcars.csv")
#' vis_drake_graph(config)
#' # See also `knitr_input()`. `knitr_input()` is like `file_input()`
#' # except that it analyzes active code chunks in your `knitr`
#' # source file and detects non-file dependencies.
#' # That way, updates to the right dependencies trigger rebuilds
#' # in your report.
#' })
#' }
file_input <- function(...){
  as.list(match.call(expand.dots = FALSE)$...) %>%
    lapply(FUN = wide_deparse) %>%
    drake_unquote
}

#' @title Declare the file outputs of a workflow plan command.
#' @description Use this function to help write the commands
#'   in your workflow plan data frame.
#' @export
#' @seealso file_input knitr_input
#' @return A character vector of declared files.
#' @param ... Symbols or character vectors denoting file outputs.
#' @examples
#' \dontrun{
#' test_with_dir("Contain side effects", {
#' # The `file_output()` and `file_input()` functions
#' # just takes in strings and returns them.
#' file_output("summaries.txt")
#' # Their main purpose is to orchestrate your custom files
#' # in your workflow plan data frame.
#' suppressWarnings(
#'   plan <- drake_plan(
#'     write.csv(mtcars, file_output("mtcars.csv")),
#'     contents = read.csv(file_input("mtcars.csv")),
#'     strings_in_dots = "literals" # deprecated but useful: no single quotes needed. # nolint
#'   )
#' )
#' plan
#' # Drake knows "\"mtcars.csv\"" is the first target
#' # and a dependency of `contents`. See for yourself:
#' config <- make(plan)
#' file.exists("mtcars.csv")
#' vis_drake_graph(config)
#' # See also `knitr_input()`. `knitr_input()` is like `file_input()`
#' # except that it analyzes active code chunks in your `knitr`
#' # source file and detects non-file dependencies.
#' # That way, updates to the right dependencies trigger rebuilds
#' # in your report.
#' })
#' }
file_output <- file_input

#' @title Declare the `knitr`/`rmarkdown` source files
#'   of a workflow plan command.
#' @description Use this function to help write the commands
#'   in your workflow plan data frame.
#' @export
#' @seealso file_input file_output
#' @return A character vector of declared files.
#' @param ... Symbols or character vectors naming source files
#'   for `knitr`/`rmarkdown` dynamic reports.
#' @examples
#' #' \dontrun{
#' test_with_dir("Contain side effects", {
#' # `knitr_input()` is like `file_input()`
#' # except that it analyzes active code chunks in your `knitr`
#' # source file and detects non-file dependencies.
#' # That way, updates to the right dependencies trigger rebuilds
#' # in your report.
#' # The basic example (`drake_example("basic")`)
#' # already has a demonstration
#' load_basic_example()
#' config <- make(my_plan)
#' vis_drake_graph(config)
#' # Now how did drake magically know that
#' # `small`, `large`, and `coef_regression2_small` were
#' # dependencies of the output file `report.md`?
#' # because the command in the workflow plan had
#' # `knitr_input("report.Rmd")` in it, so drake knew
#' # to analyze the active code chunks. There, it spotted
#' # where `small`, `large`, and `coef_regression2_small`
#' # were read from the cache using calls to `loadd()` and `readd()`.
#' })
#' }
knitr_input <- file_input

code_dependencies <- function(expr){
  if (
    !is.function(expr) &&
    !is.expression(expr) &&
    !is.language(expr)
  ){
    return(list())
  }

  results <- list()

  # `walk()` sees `results` in its lexical scope.
  walk <- function(
    expr,
    knitr_input = FALSE,
    file_input = FALSE,
    file_output = FALSE
  ){
    if (!length(expr)){
      return()
    } else if (is.function(expr)) {
      expr <- unwrap_function(expr)
      if (typeof(expr) != "closure"){
        expr <- function(){} # nolint: curly braces are necessary
      }
      walk(
        body(expr),
        knitr_input = knitr_input,
        file_input = file_input,
        file_output = file_output
      )
    } else if (is.name(expr) || is.atomic(expr)) {
      if (knitr_input){
        file <- declare_file(expr)
        results$knitr_input <<- base::union(results$knitr_input, file)
        results <<- merge_lists(x = results, y = knitr_deps_list(file))
      } else if (file_input){
        results$file_input <<- base::union(
          results$file_input,
          declare_file(expr)
        )
      } else if (file_output) {
        results$file_output <<- base::union(
          results$file_output,
          declare_file(expr)
        )
      } else {
        results$globals <<- base::union(
          results$globals,
          setdiff(wide_deparse(expr), drake_fn_patterns)
        )
      }
    } else if (is.call(expr) || is.recursive(expr)) {
      if (is_loadd_call(expr)){
        results$loadd <<- base::union(
          results$loadd,
          setdiff(analyze_loadd(expr), drake_fn_patterns)
        )
      } else if (is_readd_call(expr)){
        results$readd <<- base::union(
          results$readd,
          setdiff(analyze_readd(expr), drake_fn_patterns)
        )
      }
      if (wide_deparse(expr[[1]]) %in% c("::", ":::")){
        results$namespaced <<- base::union(
          results$namespaced,
          setdiff(wide_deparse(expr), drake_fn_patterns)
        )
      } else {
        lapply(
          X = expr,
          FUN = walk,
          knitr_input = is_knitr_input_call(expr),
          file_input = is_file_input_call(expr),
          file_output = is_file_output_call(expr)
        )
      }
    }
  }
  # Actually walk the expression.
  walk(expr)
  # Filter out useless globals.
  if (length(results$globals)){
    results$globals <- intersect(results$globals, find_globals(expr))
  }
  results
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
    setdiff(y = formals) %>%
    Filter(f = is_parsable)
}

declare_file <- function(expr){
  out <- wide_deparse(expr) %>%
    drake_unquote %>%
    setdiff(y = drake_fn_patterns)
  if (length(out)){
    drake_quotes(out, single = FALSE)
  } else {
    character(0)
  }
}

analyze_loadd <- function(expr){
  args <- as.list(expr)[-1]
  targets <- unnamed_in_list(args)
  list <- get_specific_arg(args = args, name = "list")
  c(targets, list)
}

analyze_readd <- function(expr){
  args <- as.list(expr)[-1]
  targets <- unnamed_in_list(args)
  target <- get_specific_arg(args = args, name = "target")
  c(targets, target)
}

get_specific_arg <- function(args, name){
  tryCatch(
    eval(args[[name]]),
    error = error_character0
  )
}

unnamed_in_list <- function(x){
  if (!length(names(x))){
    as.character(x)
  } else {
    as.character(x[!nchar(names(x))])
  }
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
