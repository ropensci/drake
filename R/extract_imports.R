#' @title Expose all the imports in a package so `make()` can detect
#'   all the package's nested functions.
#' @export
expose_imports <- function(
  package,
  character_only = FALSE,
  envir = parent.frame(),
  jobs = 1
){
  if (!character_only){
    package <- as.character(substitute(package))
  }
  pkg_env <- getNamespace(package) %>%
    as.list(all.names = TRUE) %>%
    list2env(parent = globalenv())
  lightly_parallelize(
    X = ls(pkg_env, all.names = TRUE),
    FUN = function(name){
      assign(
        x = name,
        envir = envir,
        value = `environment<-`(get(name, envir = pkg_env), pkg_env)
      )
    },
    jobs = jobs
  )
  envir
}
