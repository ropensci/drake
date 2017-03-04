run_Makefile = function(config, run = TRUE){
  config$cache$set("config", config, namespace = "makefile")
  makefile = file.path(cachepath, "Makefile")
  sink("Makefile")
  makefile_head(config)
  makefile_rules(config)
  sink() 
  initialize(config)
  if(run) system2(command = config$command, args = config$args)
  invisible()
}

#' @title Internal function \code{default_system2_args}
#' @description Internal function to configure 
#' arguments to \code{\link{system2}()} to run Makefiles.
#' Not a user-side function.
#' @export
#' @return \code{args} for \code{\link{system2}(command, args)}
#' @param jobs number of jobs
#' @param logical, whether to be verbose
default_system2_args = function(jobs, verbose){
  out = paste0("--jobs=", jobs)
  if(!verbose) out = c(out, "--silent")
  out
}

makefile_head = function(config){
  if(length(config$prepend)) cat(config$prepend, "\n", sep = "\n")
  cat("all: ", timestamp(config$targets), sep = " \\\n")
}

makefile_rules = function(config){
  targets = intersect(config$plan$target, config$order)
  for(target in targets){
    deps = dependencies(target, config) %>%
      intersect(y = config$plan$target) %>% timestamp
    breaker = ifelse(length(deps), " \\\n", "\n")
    cat("\n", timestamp(target), ":", breaker, sep = "")
    if(length(deps)) cat(deps, sep = breaker)
    if(is_file(target)) 
      target = paste0("drake::as_file(\"", eply::unquote(target), "\")")
    else target = quotes(unquote(target), single = FALSE)
    cat("\tRscript -e 'drake::mk(", target, ")'\n", sep = "")
  }
}

initialize = function(config){ 
  config$cache$clear(namespace = "status")
  for(code in config$prework) eval(parse(text = code), envir = config$envir)
  imports = setdiff(config$order, config$plan$target)
  lapply(imports, build, config = config)
  timestamps(config)
  invisible()
}

#' @title Function \code{mk}
#' @description Internal drake function to be called 
#' inside Makefiles only. Makes a single target.
#' Users, do not invoke directly.
#' @export
#' @param target name of target to make
mk = function(target){
  config = get_cache()$get("config", namespace = "makefile")
  for(code in config$prework)
    suppressPackageStartupMessages(
      eval(parse(text = code), envir = config$envir))
  load_dependencies(targets = target, config = config)
  build(target = target, config = config)
  file_overwrite(timestamp(target))
  invisible()
}
