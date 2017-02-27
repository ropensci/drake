makefile = function(plan, target, verbose, envir, command, args,
    run, prepend, packages, global, force_rehash){
  force(envir)
  if("all" %in% plan$target)
    stop("\"all\" cannot be in plan$target.")  
  x = setup(plan = plan, target = target, verbose = verbose,
    envir = envir, force_rehash = force_rehash,
    run = run, prepend = prepend, global = global,
    command = command)
  x$cache$set("packages", packages, namespace = "makefile")
  x$cache$set("global", global, namespace = "makefile")
  x$cache$set("plan", plan, namespace = "makefile")
  makefile = file.path(cachepath, "Makefile")
  sink("Makefile")
  plan = x$plan[complete.cases(x$plan),]
  makefile_head(prepend = prepend, targets = plan$target)
  makefile_rules(plan, 
    verbose = verbose, force_rehash = force_rehash)
  sink()
  initialize(x)
  if(run) system2(command = command, args = args)
  invisible()
}

#' @title Function \command{as_file}
#' @description Converts an ordinary character string
#' into a filename understandable by drake. In other words,
#' \command{as_file(x)} just wraps single quotes around \command{x}
#' @export
#' @return a single-quoted character string: i.e., a filename
#' understandable by drake.
#' @param x character string to be turned into a filename
#' understandable by drake (i.e., a string with literal
#' single quotes on both ends).
as_file = function(x){
  eply::quotes(x, single = TRUE)
}

makefile_head = function(prepend, targets){
  if(length(prepend)){
    cat(prepend, sep = "\n")
    cat("\n")
  }
  cat("all: ", timestamp(targets), "\n")
}

makefile_rules = function(plan, verbose, force_rehash){
  y = Make$new(plan, envir = new.env(), target = plan$target)
  for(x in plan$target){
    cat("\n", timestamp(x), ": ", sep = "")
    deps = intersect(y$deps(x), plan$target) %>% timestamp
    cat(deps, "\n")
    if(is_file(x)) 
      x = paste0("drake::as_file(\"", eply::unquote(x), "\")")
    else x = quotes(unquote(x), single = FALSE)
    cat("\tRscript -e 'drake::build(", x, 
      ", verbose = ", verbose, ", force_rehash = ", force_rehash, 
      ")'\n", sep = "")
  }
}

initialize = function(x){
  packages = x$cache$get("packages", namespace = "makefile")
  for(package in packages) 
    if(!isPackageLoaded(package))
      require(package, character.only = TRUE)
  x$cache$get("global", namespace = "makefile") %>%
    eply::evals(.with = globalenv())
  imports = x$plan$target[is.na(x$plan$command)]
  x$cache$clear(namespace = "status")
  uncache_imported(x$cache)
  for(i in imports) x$update(i)
  timestamps(x)
  invisible()
}

#' @title Internal function \command{build}
#' @description Builds an individual target 
#' inside the \command{Makefiles} created by 
#' \command{\link{run}(..., makefile = TRUE)}.  
#' Not meant to be called by the user.
#' @export
#' @param target name of target to make
#' @param verbose logical, same as in \command{link{run}()}
#' @param force_rehash logical, same as with \command{\link{run}()}
build = function(target, verbose, force_rehash){
  cache = storr_rds(cachepath, mangle_key = TRUE)
  plan = cache$get("plan", namespace = "makefile")
  imported = imported() %>% Filter(f = is_not_file)
  imports = lapply(imported, readd, character_only = TRUE) 
  names(imports) = imported
  envir = list2env(imports, parent = globalenv())
  x = Make$new(plan = plan, verbose = verbose, envir = envir,
    target = target, force_rehash = force_rehash)
  packages = x$cache$get("packages", namespace = "makefile")
  for(package in packages) 
    if(!isPackageLoaded(package))
      suppressPackageStartupMessages(
        require(package, character.only = TRUE))
  x$cache$get("global", namespace = "makefile") %>%
    eply::evals(.with = globalenv())
  x$make(clear_status = FALSE)
  file_overwrite(timestamp(target))
  invisible()
}
