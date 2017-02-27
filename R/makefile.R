run_makefile = function(args){
  args$cache$set("args", args, namespace = "makefile")
  makefile = file.path(cachepath, "Makefile")
  sink("Makefile")
  makefile_head(args)
  makefile_rules(args)
  sink()
  initialize(args)
  make_args = paste0("--jobs=", args$jobs)
  if(!args$verbose) make_args = paste(make_args, "--silent")
  system2(command = args$make, args = c(args$args, make_args))
  invisible()
}

makefile_head = function(args){
  if(length(args$prepend)) cat(args$prepend, "\n", sep = "\n")
  cat("all: ", timestamp(args$targets), "\n", sep = "\n")
}

makefile_rules = function(args){
  for(x in args$plan$target){
    cat("\n", timestamp(x), ": ", sep = "")
    deps = graphical_dependencies(x, args$graph) %>%
      intersect(y = args$plan$target) %>% timestamp
    cat(deps, "\n", sep = "\n")
    if(is_file(x)) 
      x = paste0("drake::as_file(\"", eply::unquote(x), "\")")
    else x = quotes(unquote(x), single = FALSE)
    cat("\tRscript -e 'drake::mk(", x, "\n")
  }
}

initialize = function(args){ 
  args$cache$clear(namespace = "status")
  uncache_imported(x$cache)
  evals(args$prework, .with = args$envir)
  imports = setdiff(V(args$graph)$name, args$plan$target)
  for(import in imports) build(import, args)
  timestamps(x)
  invisible()
}

#' @title Function \code{mk}
#' @description Internal drake function to be called 
#' inside Makefiles only. Makes a single target.
#' Users, do not invoke directly.
#' export
#' @param target name of target to make
mk = function(target){
  args = get_cache()$get("args", namespace = "makefile")
  build(target = target, args = args)
}
