makefile = function(plan, output, verbose, envir, command, args,
    run, prepend, packages, global, force_rehash){
  force(envir)
  if("all" %in% plan$output)
    stop("\"all\" cannot be in plan$output.")  
  x = setup(plan = plan, output = output, verbose = verbose,
    envir = envir, force_rehash = force_rehash,
    run = run, prepend = prepend, global = global,
    command = command)
  x$cache$set("packages", packages, namespace = "makefile")
  x$cache$set("global", global, namespace = "makefile")
  x$cache$set("plan", plan, namespace = "makefile")
  makefile = file.path(cache_path, "Makefile")
  sink("Makefile")
  plan = x$plan[complete.cases(x$plan),]
  makefile_head(prepend = prepend, targets = plan$output)
  makefile_rules(plan, 
    verbose = verbose, force_rehash = force_rehash)
  sink()
  initialize(x)
  if(run) system2(command = command, args = args)
  invisible()
}

#' @title Function \code{as_file}
#' @description Converts an ordinary character string
#' into a filename understandable by drake. In other words,
#' \code{as_file(x)} just wraps single quotes around \code{x}
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
  y = Make$new(plan, envir = new.env(), output = plan$output)
  for(x in plan$output){
    cat("\n", timestamp(x), ": ", sep = "")
    deps = intersect(y$deps(x), plan$output) %>% timestamp
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
  imports = x$plan$output[is.na(x$plan$code)]
  x$cache$clear(namespace = "status")
  uncache_imported(x$cache)
  for(i in imports) x$update(i)
  timestamps(x)
  invisible()
}

#' @title Internal function \code{build}
#' @description Builds an individual output 
#' inside the \code{Makefiles} created by 
#' \code{\link{make}(..., makefile = TRUE)}.  
#' Not meant to be called by the user.
#' @seealso \code{link{help_drake}}
#' @export
#' @param output name of output to make
#' @param verbose logical, same as in \code{link{make}()}
#' @param force_rehash logical, same as with \code{\link{make}()}
build = function(output, verbose, force_rehash){
  cache = storr_rds(cache_path, mangle_key = TRUE)
  plan = cache$get("plan", namespace = "makefile")
  imported = imported() %>% Filter(f = is_not_file)
  imports = lapply(imported, readd, character_only = TRUE) 
  names(imports) = imported
  envir = list2env(imports, parent = globalenv())
  x = Make$new(plan = plan, verbose = verbose, envir = envir,
    output = output, force_rehash = force_rehash)
  packages = x$cache$get("packages", namespace = "makefile")
  for(package in packages) 
    if(!isPackageLoaded(package))
      suppressPackageStartupMessages(
        require(package, character.only = TRUE))
  x$cache$get("global", namespace = "makefile") %>%
    eply::evals(.with = globalenv())
  x$make(clear_status = FALSE)
  file_overwrite(timestamp(output))
  invisible()
}
