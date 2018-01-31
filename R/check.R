#' @title Check a workflow plan data frame for obvious errors.
#' @description Possible obvious errors include
#' circular dependencies and
#' missing input files.
#' @seealso \code{ink{drake_plan}}, [make()]
#' @export
#' @return Invisibly return `plan`.
#' @param plan workflow plan data frame, possibly from
#' [drake_plan()].
#' @param targets character vector of targets to make
#' @param envir environment containing user-defined functions
#' @param cache optional drake cache. See [new_cache()]
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
  plan = drake_plan(),
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
  check_strings(config$plan, jobs = jobs)
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
  x <- drake_unquote(x)
  bad <- which(!is_parsable(x)) %>% names
  if (!length(bad))
    return(invisible())
  warning("Possibly bad target names:\n", multiline_message(bad),
    call. = FALSE)
  invisible()
}

check_strings <- function(plan, jobs) {
  x <- stri_extract_all_regex(plan$command, "(?<=\").*?(?=\")")
  names(x) <- plan$target
  x <- x[!is.na(x)]
  if (!length(x))
    return()
  x <- lightly_parallelize(
    X = x,
    FUN = function(y) {
      if (length(y) > 2)
        return(y[seq(from = 1, to = length(y), by = 2)])
      else
        return(y)
    },
    jobs = jobs
  )
  message("Double-quoted strings were found in plan$command.\n",
    "Should these be single-quoted instead?\n",
    "Remember: single-quoted strings are file target dependencies\n",
    "and double-quoted strings are just ordinary strings.")
  for (target in seq_len(length(x))) {
    message("\ntarget: ", names(x)[target])
    message("strings in command:\n",
      multiline_message(drake::drake_quotes(x[[target]],
      single = FALSE)), sep = "")
  }
}
