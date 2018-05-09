#' @title Expose all the imports in a package so `make()` can detect
#'   all the package's nested functions.
#' @description When `drake` analyzes the functions in your environment,
#'   it understands that some of your functions
#'   are nested inside other functions.
#'   It dives into nested function after nested function in your environment
#'   so that if an inner function changes, targets produced by the outer
#'   functions will become out of date. However, `drake` stops searching
#'   as soon as it sees a function from a package. This keeps projects
#'   from being too brittle, but it is sometimes problematic.
#'   You may want to strongly depend on a package's internals.
#'   In fact, you may want to wrap your data analysis project itself
#'   in a formal R package,
#'   so you want all your functions to be reproducibly tracked.
#'
#'   To make all a package's functions available to be tracked
#'   as dependencies, use the `expose_imports()` function.
#'   See the examples in this help file for a demonstration.
#'
#' @details Thanks to [Jasper Clarkberg](https://github.com/dapperjapper)
#'   for the idea that makes this function work.
#' @export
#' @return the environment that the exposed imports are loaded into.
#'   Defaults to your R workspace.
#' @param package name of the package, either a symbol or a string,
#'   depending on `character_only`.
#' @param character_only logical, whether to interpret `package`
#'   as a character string or a symbol (quoted vs unquoted).
#' @param envir environment to load the exposed package imports.
#'   You will later pass this `envir` to [make()].
#' @param jobs number of parallel jobs for the parallel processing
#'   of the imports.
#' @examples
#' \dontrun{
#' test_with_dir("contain this example's side effects", {
#' # Suppose you have a workflow that uses the `digest()` function,
#' # which computes the hash of an object.
#'
#' library(digest) # Has the digest() function.
#' g <- function(x){
#'   digest(x)
#' }
#' f <- function(x){
#'   g(x)
#' }
#' plan <- drake_plan(x = f(1))
#'
#' # Here are the reproducibly tracked objects in the workflow.
#' tracked(plan)
#'
#' # But the digest() function has dependencies too.
#' head(deps_code(digest))
#'
#' # Why doesn't `drake` import them? Because it knows `digest()`
#' # is from a package, and it doesn't usually dive into functions
#' # from packages. We need to call expose_imports() to expose
#' # a package's inner functions.
#'
#' expose_imports(digest)
#' new_objects <- tracked(plan)
#' head(new_objects, 10)
#' length(new_objects)
#'
#' # Now when you call `make()`, `drake` will dive into `digest`
#' # to import dependencies.
#'
#' cache <- storr::storr_environment() # just for examples
#' make(plan, cache = cache)
#' head(cached(cache = cache), 10)
#' length(cached(cache = cache))
#'
#' # Why would you want to expose a whole package like this?
#' # Because you may want to wrap up your data science project
#' # as a formal R package. In that case, `expose_imports()`
#' # tells `drake` to reproducibly track all of your code,
#' # not just the exported API functions you mention in
#' # workflow plan commands.
#'
#' # Note: if you use `digest::digest()`` instead of just `digest()`,
#' # `drake` does not dive into the function body anymore.
#' g <- function(x){
#'   digest::digest(x) # Was previously just digest()
#' }
#' tracked(plan)
#' })
#' }
expose_imports <- function(
  package,
  character_only = FALSE,
  envir = parent.frame(),
  jobs = 1
){
  force(envir)
  if (!character_only){
    package <- as.character(substitute(package))
  }
  expose_envir(from = getNamespace(package), to = envir, jobs = jobs)
}

expose_envir <- function(from, to, jobs, keep = ls(from, all.names = TRUE)){
  from <- as.list(from, all.names = TRUE)[keep] %>%
    list2env(parent = globalenv())
  lightly_parallelize(
    X = ls(from, all.names = TRUE),
    FUN = function(name){
      assign(
        x = name,
        envir = to,
        value = `environment<-`(get(name, envir = from), from)
      )
    },
    jobs = jobs
  )
  to
}
