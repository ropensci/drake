run_Makefile <- function( #nolint: we want Makefile capitalized.
  config,
  run = TRUE,
  debug = FALSE
){
  this_cache_path <- cache_path(config$cache)
  if (identical(globalenv(), config$envir)){
    save(
      list = ls(config$envir, all.names = TRUE),
      envir = config$envir,
      file = globalenv_file(this_cache_path)
    )
  }
  config$cache$set("config", config, namespace = "makefile")
  with_output_sink(
    new = "Makefile",
    code = {
      makefile_head(config)
      makefile_rules(config)
    }
  )
  out <- outdated(
    plan = config$plan,
    targets = config$targets,
    envir = config$envir,
    verbose = config$verbose,
    cache = config$cache,
    jobs = config$jobs,
    parallelism = config$parallelism,
    packages = config$packages,
    prework = config$prework
  )
  time_stamps(config = config, outdated = out)
  error_code <- ifelse(
    run,
    system2(command = config$command, args = config$args),
    0
  )
  if (!debug){
    dir <- cache_path(config$cache)
    file <- globalenv_file(dir)
    unlink(file, force = TRUE)
  }
  return(invisible(error_code))
}

makefile_head <- function(config){
  if (length(config$prepend)){
    cat(config$prepend, "\n", sep = "\n")
  }
  cache_path <- cache_path(config$cache) %>%
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
    this_recipe <- recipe(target, config$recipe_command)
    cat("\t", this_recipe, "\n", sep = "")
  }
}

recipe <- function(target, recipe_command){
  if (is_file(target)){
    target <- paste0("drake::as_file(\"", eply::unquote(target), "\")")
  } else{
    target <- eply::quotes(
      eply::unquote(target), single = FALSE)
  }
  r_recipe <- paste0("drake::mk(", target, ", \"", cache_value_macro, "\")")
  if (!safe_grepl(r_recipe_wildcard(), recipe_command)){
    recipe_command <- paste0(recipe_command, " '", r_recipe_wildcard(), "'")
  }
  gsub(r_recipe_wildcard(), r_recipe, recipe_command)
}

#' @title Function \code{mk}
#' @description Internal drake function to be called
#' inside Makefiles only. Makes a single target.
#' Users, do not invoke directly.
#' @export
#' @param target name of target to make
#' @param cache_path path to the drake cache
mk <- function(target, cache_path = NULL){
  cache <- this_cache(cache_path)
  config <- cache$get("config", namespace = "makefile")
  if (identical(globalenv(), config$envir)){
    dir <- cache_path
    file <- globalenv_file(dir)
    load(file = file, envir = config$envir)
  }
  config <- inventory(config)
  do_prework(config = config, verbose_packages = FALSE)
  prune_envir(targets = target, config = config)
  hash_list <- hash_list(targets = target, config = config)
  old_hash <- self_hash(target = target, config = config)
  current <- target_current(target = target,
    hashes = hash_list[[target]], config = config)
  if (current){
    return(invisible())
  }
  build(
    target = target,
    hash_list = hash_list,
    config = config
  )
  config <- inventory(config)
  new_hash <- self_hash(target = target, config = config)
  if (!identical(old_hash, new_hash)){
    file <- time_stamp_file(target = target, config = config)
    file_overwrite(file)
  }
  return(invisible())
}

#' @title Function \code{default_system2_args}
#' @description Configures default
#' arguments to \code{\link{system2}()} to run Makefiles.
#' @export
#' @return \code{args} for \code{\link{system2}(command, args)}
#' @param jobs number of jobs
#' @param verbose logical, whether to be verbose
#' @examples
#' default_system2_args(jobs = 2, verbose = FALSE)
#' default_system2_args(jobs = 4, verbose = TRUE)
default_system2_args <- function(jobs, verbose){
  out <- paste0("--jobs=", jobs)
  if (!verbose){
    out <- c(out, "--silent")
  }
  return(out)
}

#' @title Function \code{default_Makefile_command}
#' @description Give the default \code{command}
#' argument to \code{\link{make}()}
#' @export
#' @examples
#' default_Makefile_command()
default_Makefile_command <- function(){
  "make"
}

#' @title default_recipe_command
#' @export
#' @seealso \code{\link{r_recipe_wildcard}}
#' @description Function to give the default
#' recipe command for Makefile parallelism.
#' @details Makefile recipes to build targets are customizable.
#' The default recipe is \code{Rscript -e 'R_RECIPE'}, where
#' \code{R_RECIPE} is the wildcard for the recipe in R for making the target.
#' In writing the Makefile, \code{R_RECIPE} is replaced with something like
#' \code{drake::mk("name_of_target", "path_to_cache")}.
#' So when you call
#' \code{make(..., parallelism = "Makefile", recipe_command = "R -e 'R_RECIPE' -q")}, # nolint
#' from within R, the \code{Makefile} builds each target
#' with the \code{Makefile} recipe,
#' \code{R -e 'drake::mk("this_target", "path_to_cache")' -q}.
#' But since \code{R -q -e} fails on Windows,
#' so the default \code{recipe_command} argument is
#' \code{"Rscript -e 'R_RECIPE'"}
#' (equivalently just \code{"Rscript -e"}),
#' so the default \code{Makefile} recipe for each target is
#' \code{Rscript -e 'drake::mk("this_target", "path_to_cache")'}.
#' @examples
#' default_recipe_command()
#' r_recipe_wildcard()
#' \dontrun{
#' load_basic_example()
#' # Look at the Makefile generated by the following.
#' make(my_plan, paralleliem = "Makefile")
#' # Generates a Makefile with "R -q -e" rather than
#' # "Rscript -e".
#' # Be aware the R -q -e fails on Windows.
#' make(my_plan, parallelism = "Makefile", jobs = 2
#'   recipe_command = "R -q -e")
#' # Same thing:
#' clean()
#' make(my_plan, parallelism = "Makefile", jobs = 2,
#'   recipe_command = "R -q -e 'R_RECIPE'")
#' clean()
#' make(my_plan, parallelism = "Makefile", jobs = 2,
#'   recipe_command = "R -e 'R_RECIPE' -q")
#' }
default_recipe_command <- function(){
  "Rscript -e 'R_RECIPE'"
}

#' @title r_recipe_wildcard
#' @export
#' @seealso \code{\link{default_recipe_command}}
#' @description Function to give the R recipe wildcard
#' for Makefiles.
#' @details Makefile recipes to build targets are customizable.
#' The default recipe is \code{Rscript -e 'R_RECIPE'}, where
#' \code{R_RECIPE} is the wildcard for the recipe in R for making the target.
#' In writing the Makefile, \code{R_RECIPE} is replaced with something like
#' \code{drake::mk("name_of_target", "path_to_cache")}.
#' So when you call
#' \code{make(..., parallelism = "Makefile", recipe_command = "R -e 'R_RECIPE' -q")}, # nolint
#' from within R, the \code{Makefile} builds each target
#' with the \code{Makefile} recipe,
#' \code{R -e 'drake::mk("this_target", "path_to_cache")' -q}.
#' But since \code{R -q -e} fails on Windows,
#' the default \code{recipe_command} argument is
#' \code{"Rscript -e 'R_RECIPE'"}
#' (equivalently just \code{"Rscript -e"}),
#' so the default \code{Makefile} recipe for each target is
#' \code{Rscript -e 'drake::mk("this_target", "path_to_cache")'}.
#' @examples
#' default_recipe_command()
#' r_recipe_wildcard()
#' \dontrun{
#' load_basic_example()
#' # Look at the Makefile generated by the following.
#' make(my_plan, paralleliem = "Makefile")
#' # Generates a Makefile with "R -q -e" rather than
#' # "Rscript -e".
#' # Be aware the R -q -e fails on Windows.
#' make(my_plan, parallelism = "Makefile", jobs = 2
#'   recipe_command = "R -q -e")
#' # Same thing:
#' clean()
#' make(my_plan, parallelism = "Makefile", jobs = 2,
#'   recipe_command = "R -q -e 'R_RECIPE'")
#' clean()
#' make(my_plan, parallelism = "Makefile", jobs = 2,
#'   recipe_command = "R -e 'R_RECIPE' -q")
#' }
r_recipe_wildcard <- function(){
  "R_RECIPE"
}
