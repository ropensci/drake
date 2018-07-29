run_Makefile <- function( #nolint: we want Makefile capitalized.
  config,
  run = TRUE,
  debug = FALSE
){
  prepare_distributed(config = config)
  write_makefile(config = config)
  time_stamps(config = config)
  error_code <- ifelse(
    run,
    system2(command = config$command, args = config$args),
    0
  )
  if (!debug){
    finish_distributed(config = config)
  }
  return(invisible(config))
}

write_makefile <- function(config){
  assert_no_conflicting_makefile(config)
  with_output_sink(
    new = config$makefile_path,
    code = {
      makefile_head(config)
      makefile_rules(config)
    }
  )
}

assert_no_conflicting_makefile <- function(config){
  if (file.exists(config$makefile_path)){
    top_lines <- readLines(con = config$makefile_path, n = 2)
    if (!identical(top_lines, makefile_top_lines)){
      stop(
        "Makefile at `", config$makefile_path, "` already exists ",
        "and was not created by drake. You must either: \n  (1) ",
        "Remove the file `", config$makefile_path, "` or \n  (2) ",
        "Set the `parallelism` argument of drake::make() ",
        "to a value other than \"Makefile\", along with the right `args`. ",
        "Example: `make(parallelism = \"Makefile\", makefile_path = \".drake/.makefile\", command = \"make\", args = \"--file=.drake/.makefile\")` ", # nolint
        "See `?parallelism_choices` for your options ",
        "for the `parallelism` argument to `make()`.",
        call. = FALSE
      )
    }
  }
}

makefile_head <- function(config){
  cat(makefile_top_lines, sep = "\n")
  cat("\n")
  if (length(config$prepend)){
    cat(config$prepend, "\n", sep = "\n")
  }
  cache_path <- config$cache_path %>%
    to_unix_path
  cat(cache_macro, "=", cache_path, "\n\n", sep = "")
  cat(
    "all:",
    time_stamp_target(config$targets, config = config),
    sep = " \\\n"
  )
}

makefile_rules <- function(config){
  targets <- intersect(config$plan$target, V(config$graph)$name)
  cache_path <- cache_path(config$cache)
  for (target in targets){
    deps <- dependencies(target, config) %>%
      intersect(y = config$plan$target) %>%
      time_stamp_target(config = config)
    breaker <- ifelse(length(deps), " \\\n", "\n")
    cat(
      "\n",
      time_stamp_target(
        target = target,
        config = config
      ),
      ":",
      breaker,
      sep = ""
    )
    if (length(deps)){
      cat(deps, sep = breaker)
    }
    this_recipe <- build_recipe(target, config$recipe_command)
    cat("\t", this_recipe, "\n", sep = "")
  }
}

build_recipe <- function(target, recipe_command,
  cache_path = NULL){
  if (is.null(cache_path)){
    cache_path <- cache_value_macro
  }
  target <- drake::drake_quotes(
    drake::drake_unquote(target), single = FALSE)
  r_recipe <- paste0("drake::mk(target = ", target,
    ", cache_path = \"", cache_path, "\")")
  if (!safe_grepl(r_recipe_wildcard(), recipe_command, fixed = TRUE)){
    recipe_command <- paste0(recipe_command, " '",
      r_recipe_wildcard(), "'")
  }
  gsub(r_recipe_wildcard(), r_recipe, recipe_command, fixed = TRUE)
}

#' @title Build a target inside a `Makefile`
#'   during `make(..., parallelism = "Makefile")`.
#' @description Users should not need to call this function directly.
#' @export
#' @keywords internal
#' @return `NULL`
#' @param target name of target to make
#' @param cache_path path to the drake cache
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # This function is meant to be part of Makefile recipes for
#' # make(..., parallelism = "Makefile").
#' # These examples peer into the internals of drake,
#' # but are not really of practical use for most users.
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' config <- drake_config(my_plan) # Internal configuration list.
#' # Prepare to use a distributed computing parallel backend
#' # such as "Makefile" or "future_lapply".
#' # The following happens during make().
#' store_drake_config(config = config)
#' prepare_distributed(config = config)
#' # Write the dummy timestamp files usually written at the beginning
#' # of make(..., parallelism = "Makefile").
#' time_stamps(config = config)
#' # Use mk() to build a target. Usually called inside a Makefile recipe.
#' mk(target = "small", cache_path = default_cache_path())
#' })
#' }
mk <- function(
  target = character(0),
  cache_path = drake::default_cache_path()
){
  config <- recover_drake_config(cache_path)
  old_hash <- makefile_hash(target = target, config = config)
  build_distributed(target = target, cache_path = cache_path)
  new_hash <- makefile_hash(target = target, config = config)
  if (!identical(old_hash, new_hash)){
    file <- time_stamp_file(target = target, config = config)
    file_overwrite(file)
  }
  invisible()
}

makefile_hash <- function(target, config){
  paste(
    self_hash(target, config),
    output_file_hash(target = target, config = config)
  )
}

#' @title Return the default value of the
#'   `args` argument to [make()].
#' @description For `make(..., parallelism = "Makefile")`,
#' this function configures the default
#' arguments to [system2()].
#' It is an internal function, and most users do not need to
#' worry about it.
#' @export
#' @return `args` for `system2(command, args)`
#' @inheritParams drake_config
#' @param jobs number of jobs
#' @examples
#' default_Makefile_args(jobs = 2, verbose = FALSE)
#' default_Makefile_args(jobs = 4, verbose = TRUE)
default_Makefile_args <- function(jobs, verbose){
  out <- paste0("--jobs=", targets_setting(jobs))
  if (verbose < 1){
    out <- c(out, "--silent")
  }
  return(out)
}

#' @title Give the default `command`
#'   argument to [make()].
#' @description Relevant for
#' `"Makefile"` parallelism only.
#' @return A character scalar naming a Linux/Unix command
#'   to run a Makefile.
#' @export
#' @examples
#' default_Makefile_command()
default_Makefile_command <- function(){
  "make"
}

cache_macro <- "DRAKE_CACHE"
cache_value_macro <- paste0("$(", cache_macro, ")")
makefile_top_lines <- c(
  "# Created by the drake R package for internal use only.",
  "# Do not run it yourself or modify it by hand."
)

globalenv_file <- function(cache_path){
  file.path(cache_path, "globalenv.RData")
}

to_unix_path <- function(x){
  gsub("\\\\", "/", x)
}
