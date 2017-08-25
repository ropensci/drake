run_Makefile = function(config, run = TRUE, debug = FALSE){
  if(identical(globalenv(), config$envir))
    save(list = ls(config$envir, all.names = TRUE), envir = config$envir,
      file = globalenvpath)
  config$cache$set("config", config, namespace = "makefile")
  makefile = file.path(cache_dir, "Makefile")
  sink("Makefile")
  makefile_head(config)
  makefile_rules(config)
  sink()
  out = outdated(plan = config$plan, targets = config$targets,
    envir = config$envir, verbose = config$verbose, jobs = config$jobs,
    parallelism = config$parallelism, packages = config$packages,
    prework = config$prework)
  time_stamps(config, outdated = out)
  if(run) system2(command = config$command, args = config$args)
  if(!debug) unlink(globalenvpath)
  invisible()
}

#' @title Internal function \code{default_system2_args}
#' @description Internal function to configure 
#' arguments to \code{\link{system2}()} to run Makefiles.
#' Not a user-side function.
#' @export
#' @return \code{args} for \code{\link{system2}(command, args)}
#' @param jobs number of jobs
#' @param verbose logical, whether to be verbose
#' @examples
#' default_system2_args(jobs = 2, verbose = FALSE)
#' default_system2_args(jobs = 4, verbose = TRUE)
default_system2_args = function(jobs, verbose){
  out = paste0("--jobs=", jobs)
  if(!verbose) out = c(out, "--silent")
  out
}

makefile_head = function(config){
  if(length(config$prepend)) cat(config$prepend, "\n", sep = "\n")
  cat("all:", time_stamp(config$targets), sep = " \\\n")
}

makefile_rules = function(config){
  targets = intersect(config$plan$target, V(config$graph)$name)
  for(target in targets){
    deps = dependencies(target, config) %>%
      intersect(y = config$plan$target) %>% time_stamp
    breaker = ifelse(length(deps), " \\\n", "\n")
    cat("\n", time_stamp(target), ":", breaker, sep = "")
    if(length(deps)) cat(deps, sep = breaker)
    if(is_file(target)) 
      target = paste0("drake::as_file(\"", unquote(target), "\")")
    else target = quotes(unquote(target), single = FALSE)
    cat("\tRscript -e 'drake::mk(", target, ")'\n", sep = "")
  }
}

#' @title Function \code{mk}
#' @description Internal drake function to be called 
#' inside Makefiles only. Makes a single target.
#' Users, do not invoke directly.
#' @export
#' @param target name of target to make
mk = function(target){
  config = get_cache()$get("config", namespace = "makefile")
  if(identical(globalenv(), config$envir))
    load(file = globalenvpath, envir = config$envir)
  config = inventory(config)
  do_prework(config = config, verbose_packages = FALSE)
  prune_envir(targets = target, config = config)
  hash_list = hash_list(targets = target, config = config)
  old_hash = self_hash(target = target, config = config)
  current = target_current(target = target, 
    hashes = hash_list[[target]], config = config)
  if(current) return(invisible())
  build(target = target, 
    hash_list = hash_list, config = config)
  config = inventory(config)
  new_hash = self_hash(target = target, config = config)
  if(!identical(old_hash, new_hash)){
    file_overwrite(time_stamp(target))
  }
  invisible()
}
