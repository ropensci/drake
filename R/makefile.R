run_makefile = function(args, debug = FALSE){
  args$cache$set("args", args, namespace = "makefile")
  makefile = file.path(cachepath, "Makefile")
  sink("Makefile")
  makefile_head(args)
  makefile_rules(args)
  sink() 
  initialize(args)
  if(!debug) system2(command = args$command, args = args$args)
  invisible()
}

default_system2_args = function(jobs, verbose){
  out = paste0("--jobs=", jobs)
  if(verbose) out = c(out, "--silent")
  out
}

makefile_head = function(args){
  if(length(args$prepend)) cat(args$prepend, "\n", sep = "\n")
  cat("all: ", timestamp(args$targets), sep = " \\\n")
}

makefile_rules = function(args){
  for(x in args$plan$target){
    deps = dependencies(x, args) %>%
      intersect(y = args$plan$target) %>% timestamp
    breaker = ifelse(length(deps), " \\\n", "\n")
    cat("\n", timestamp(x), ":", breaker, sep = "")
    if(length(deps)) cat(deps, sep = breaker)
    if(is_file(x)) 
      x = paste0("drake::as_file(\"", eply::unquote(x), "\")")
    else x = quotes(unquote(x), single = FALSE)
    cat("\tRscript -e 'drake::mk(", x, ")'\n", sep = "")
  }
}

initialize = function(args){ 
  args$cache$clear(namespace = "status")
  for(code in args$prework) eval(parse(text = code), envir = args$envir)
  imports = setdiff(args$order, args$plan$target)
  lapply(imports, build, args = args)
  timestamps(args)
  invisible()
}

#' @title Function \code{mk}
#' @description Internal drake function to be called 
#' inside Makefiles only. Makes a single target.
#' Users, do not invoke directly.
#' @export
#' @param target name of target to make
mk = function(target){
  args = get_cache()$get("args", namespace = "makefile")
  for(code in args$prework) eval(parse(text = code), envir = args$envir)
  prune_envir(target, args)
  build(target = target, args = args)
}
