#' @title Check a workflow plan data frame for obvious errors.
#' @description Possible obvious errors include
#' circular dependencies and
#' missing input files.
#' @seealso \code{ink{drake_plan}}, [make()]
#' @export
#' @return Invisibly return `plan`.
#' @param plan workflow plan data frame, possibly from
#'   [drake_plan()].
#' @param targets character vector of targets to make
#' @param envir environment containing user-defined functions
#' @param cache optional drake cache. See [new_cache()].
#' @param verbose same as for [make()]
#' @param jobs number of jobs/workers for light parallelism
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' check_plan(my_plan) # Check the workflow plan dataframe for obvious errors.
#' unlink('report.Rmd') # Remove an import file mentioned in the plan.
#' # If you un-suppress the warnings, check_plan()
#' # will tell you that 'report.Rmd' is missing.
#' suppressWarnings(check_plan(my_plan))
#' })
#' }
check_plan <- function(
  plan = read_drake_plan(),
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  cache = drake::get_cache(verbose = verbose),
  verbose = TRUE,
  jobs = 1
){
  force(envir)
  config <- drake_config(
    plan = plan,
    targets = targets,
    envir = envir,
    verbose = verbose,
    cache = cache,
    jobs = jobs
  )
  check_drake_config(config)
  invisible(plan)
}

check_drake_config <- function(config) {
  if (config$skip_safety_checks){
    return(invisible())
  }
  stopifnot(is.data.frame(config$plan))
  if (!all(c("target", "command") %in% colnames(config$plan)))
    stop("The columns of your workflow plan data frame ",
      "must include 'target' and 'command'.")
  stopifnot(nrow(config$plan) > 0)
  stopifnot(length(config$targets) > 0)
  missing_input_files(config = config)
  assert_standard_columns(config = config)
  warn_bad_symbols(config$plan$target)
  parallelism_warnings(config = config)
  check_drake_graph(graph = config$graph)
}

assert_standard_columns <- function(config){
  x <- setdiff(colnames(config$plan), drake_plan_columns())
  if (length(x)){
    warning(
      "Non-standard columns in workflow plan:\n",
      multiline_message(x),
      call. = FALSE
    )
  }
}

missing_input_files <- function(config) {
  missing_files <- V(config$graph)$name %>%
    setdiff(y = config$plan$target) %>%
    parallel_filter(f = is_file, jobs = config$jobs) %>%
    drake_unquote %>%
    parallel_filter(f = function(x) !file.exists(x), jobs = config$jobs)
  if (length(missing_files))
    warning("missing input files:\n", multiline_message(missing_files),
      call. = FALSE)
  invisible(missing_files)
}

warn_bad_symbols <- function(x) {
  x <- Filter(x = x, f = is_not_file)
  bad <- which(!is_parsable(x)) %>% names
  if (!length(bad))
    return(invisible())
  warning("Possibly bad target names:\n", multiline_message(bad),
    call. = FALSE)
  invisible()
}

check_drake_graph <- function(graph){
  if (is_dag(graph)){
    return()
  }
  comp <- igraph::components(graph, mode = "strong")
  cycle_component_indices <- which(comp$csize > 1)
  cycles <- lapply(cycle_component_indices, function(i){
    names(comp$membership[comp$membership == i]) %>%
      paste(collapse = " ")
  }) %>%
    unlist
  stop(
    "Circular workflow: there is a target ",
    "that ultimately depends on itself. Cycles:\n",
    multiline_message(cycles)
  )
}

check_jobs <- function(jobs){
  stopifnot(length(jobs) > 0)
  stopifnot(is.numeric(jobs) || is.integer(jobs))
  stopifnot(all(jobs > 0))
  if (length(jobs) > 1){
    if (
      is.null(names(jobs)) ||
      !identical(sort(names(jobs)), sort(c("imports", "targets")))
    ){
      stop(
        "In the `jobs` argument, you must either give a numeric scalar ",
        "or a named numeric vector with names 'imports' and 'targets'.",
        call. = FALSE
      )
    }
  }
}
