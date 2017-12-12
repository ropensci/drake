#' @title Function \code{deps}
#' @description List the dependencies of a function or workflow plan command.
#' Or, if the argument is a single-quoted string that points to
#' a dynamic knitr report, the dependencies of the expected compiled
#' output will be given. For example, \code{deps("'report.Rmd'")}
#' will return target names found in calls to \code{\link{loadd}()}
#' and \code{\link{readd}()} in active code chunks.
#' These targets are needed in order to run \code{knit('report.Rmd')}
#' to produce the output file \code{'report.md'}, so technically,
#' they are dependencies of \code{'report.md'}, not \code{'report.Rmd'}
#' @export
#' @param x Either a function or a string.
#' Strings are commands from your workflow plan data frame.
#' @return A character vector, names of dependencies.
#' Files wrapped in single quotes.
#' The other names listed are functions or generic R objects.
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
#' my_plan <- plan_drake(
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
#' load_basic_example() # Loads the basic example and writes 'report.Rmd'.
#' # Dependencies of the knitr-generated targets like 'report.md'
#' # include targets/imports referenced with `readd()` or `loadd()`.
#' deps("'report.Rmd'")
#' })
#' }
deps <- function(x){
  if (is.function(x)){
    out <- function_dependencies(x)
  } else if (is_file(x) & file.exists(file <- drake::drake_unquote(x))){
    out <- knitr_deps(x)
  } else if (is.character(x)){
    out <- command_dependencies(x)
  } else{
    stop("x must be a character scalar or function.")
  }
  clean_dependency_list(out)
}

#' @title Function \code{dependency_profile}
#' @description Return the detailed dependency profile
#' of the target. Useful for debugging.
#' For up to date targets, like elements
#' of the returned list should agree: for example,
#' \code{cached_dependency_hash} and
#' \code{current_dependency_hash}.
#' @return A list of information that drake takes into account
#' when examining the dependencies of the target.
#' @export
#' @seealso \code{\link{deps}}, \code{\link{make}},
#' \code{\link{config}}
#' @param target name of the target
#' @param config configuration list output by
#' \code{\link{config}} or \code{\link{make}}
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load drake's canonical exmaple.
#' con <- make(my_plan) # Run the project, build the targets.
#' # Get some example dependency profiles of targets.
#' dependency_profile("small", config = con)
#' dependency_profile("'report.md'", config = con)
#' clean(large) # Clear out most of the information about the target 'large'.
#' # The dependency hashes are still there.
#' dependency_profile("large", config = con)
#' # Should agree with `$hashes_of_dependencies`.
#' con$cache$get_hash("simulate",
#'   namespace = "kernels")
#' })
#' }
dependency_profile <- function(target, config){
  config$plan[["trigger"]] <- NULL
  cached_command <- get_from_meta(
    key = target, metaspace = "command", cache = config$cache)
  current_command <- get_command(target = target, config = config)

  deps <- dependencies(target, config)
  hashes_of_dependencies <- self_hash(target = deps, config = config)
  current_dependency_hash <- digest::digest(hashes_of_dependencies,
    algo = config$long_hash_algo)
  cached_dependency_hash <- get_from_meta(
    key = target, metaspace = "depends", cache = config$cache)
  names(hashes_of_dependencies) <- deps

  cached_file_modification_time <- get_from_meta(
    key = target, metaspace = "mtime", cache = config$cache)
  current_file_modification_time <- ifelse(is_file(target),
    file.mtime(drake::drake_unquote(target)), NA)

  out <- list(
    cached_command = cached_command,
    current_command = current_command,
    cached_file_modification_time = cached_file_modification_time,
    current_file_modification_time = current_file_modification_time,
    cached_dependency_hash = cached_dependency_hash,
    current_dependency_hash = current_dependency_hash,
    hashes_of_dependencies = hashes_of_dependencies
  )
  out[!is.na(out)]
}

#' @title Function \code{tracked}
#' @description Print out which objects, functions, files, targets, etc.
#' are reproducibly tracked.
#' @export
#' @return A character vector with the names of reproducibly-tracked targets.
#' @param plan workflow plan data frame, same as for function
#' \code{\link{make}()}.
#' @param targets names of targets to build, same as for function
#' \code{\link{make}()}.
#' @param envir environment to import from, same as for function
#' \code{\link{make}()}.
#' @param jobs number of jobs to accelerate the construction
#' of the dependency graph. A light \code{mclapply}-based
#' parallelism is used if your operating system is not Windows.
#' @param verbose logical, whether to print
#' progress messages to the console.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load the canonical example for drake.
#' # List all the targets/imports that are reproducibly tracked.
#' tracked(my_plan)
#' })
#' }
tracked <- function(
  plan = plan_drake(),
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

dependencies <- function(targets, config){
  adjacent_vertices(
    graph = config$graph,
    v = targets,
    mode = "in"
    ) %>%
  lapply(FUN = names) %>%
  clean_dependency_list()
}

command_dependencies <- function(command){
  if (!length(command)){
    return()
  }
  if (is.na(command)){
    return()
  }
  command <- as.character(command) %>%
    braces()
  fun <- function(){} # nolint: I'm still not sure why these braces need to be here.
  body(fun) <- parse(text = command)
  non_files <- function_dependencies(fun) %>%
    unlist()
  files <- extract_filenames(command)
  if (length(files)){
    files <- drake::drake_quotes(files, single = TRUE)
  }
  knitr <- find_knitr_doc(command) %>%
    knitr_deps
  c(non_files, files, knitr) %>%
    clean_dependency_list
}

import_dependencies <- function(object){
  if (is.function(object)){
    function_dependencies(object) %>% clean_dependency_list
  } else{
    character(0)
  }
}

# Walk through function f and find `pkg::fun()` and `pkg:::fun()` calls.
find_namespaced_functions <- function(f, found = character(0)){
  if (is.function(f)){
    return(find_namespaced_functions(body(f), found))
  } else if (is.call(f) && deparse(f[[1]]) %in% c("::", ":::")){
    found <- c(found, deparse(f))
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

function_dependencies <- function(funct){
  funct <- unwrap_function(funct)
  if (typeof(funct) != "closure"){
    funct <- function(){} # nolint: curly braces are necessary
  }
  out <- findGlobals(funct, merge = FALSE)
  namespaced <- find_namespaced_functions(funct)
  out$functions <- c(out$functions, namespaced) %>%
    sort()
  parsable_list(out)
}

clean_dependency_list <- function(x){
  x %>%
    unlist() %>%
    unname() %>%
    unique() %>%
    sort()
}

parsable_list <- function(x){
  lapply(x, function(y) Filter(is_parsable, y))
}

is_parsable <- Vectorize(function(x){
  tryCatch({
      parse(text = x)
      TRUE
    },
    error = error_false
  )
},
"x")

extract_filenames <- function(command){
  if (!safe_grepl("'", command)){
    return(character(0))
  }
  splits <- str_split(command, "'")[[1]]
  splits[seq(from = 2, to = length(splits), by = 2)]
}

safe_grepl <- function(pattern, x){
  tryCatch(grepl(pattern, x), error = error_false)
}

is_file <- function(x){
  safe_grepl("^'", x) & safe_grepl("'$", x)
}

is_existing_file <- function(x){
  is_file(x) & file.exists(drake_unquote(x, deep = TRUE))
}

is_not_file <- function(x){
  !is_file(x)
}

tidy_command <- function(x) {
  formatR::tidy_source(
    source = NULL,
    comment = FALSE,
    blank = FALSE,
    arrow = TRUE,
    brace.newline = FALSE,
    indent = 4,
    output = FALSE,
    text = x,
    width.cutoff = 119
  )$text.tidy %>%
    paste(collapse = "\n") %>%
    braces
}

braces <- function(x) {
  paste("{\n", x, "\n}")
}

get_command <- function(target, config) {
  config$plan$command[config$plan$target == target] %>%
    tidy_command
}
