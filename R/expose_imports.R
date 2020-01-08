#' @title Expose package functions and objects for analysis with drake
#' \lifecycle{stable}
#' @description `drake` usually ignores the functions and objects
#'   from packages. Use `expose_imports()` to bring the functions and
#'   objects from a package into your environment. That way,
#'   `drake` scans them for dependencies and watches for when
#'   they change.
#' @details Thanks to [Jasper Clarkberg](https://github.com/dapperjapper)
#'   for the idea that makes this function work.
#' @export
#' @return The environment that the exposed imports are loaded into.
#'   Defaults to your R workspace.
#' @param package Name of the package, either a symbol or a string,
#'   depending on `character_only`.
#' @param character_only Logical, whether to interpret `package`
#'   as a character string or a symbol (quoted vs unquoted).
#' @param envir Environment to load the exposed package imports.
#'   You will later pass this `envir` to [make()].
#' @param jobs Number of parallel jobs for the parallel processing
#'   of the imports.
#' @examples
#' # nolint start
#' \dontrun{
#' isolate_example("contain side effects", {
#' # Consider a simple plan that depends on the biglm package.
#' # library(biglm)
#' plan <- drake_plan(model = biglm(y ~ x, data = huge_dataset))
#' # Even if you load the biglm package, drake still ignores
#' # the biglm() function as a dependency. The function is missing
#' # from the graph:
#' # vis_drake_graph(plan)
#' # And if you install an updated version of biglm with a revised
#' # biglm() function, this will not cause drake::make(plan)
#' # to rerun the model.
#' # This is because biglm() is not in your environment.
#' # ls()
#' # biglm() exists in its own special package environment,
#' # which drake does not scan.
#' # ls("package:biglm")
#' # To depend on biglm(), use expose_imports(biglm)
#' # to bring the objects and functions in biglm into
#' # your own (non-package) environment.
#' # expose_imports(biglm)
#' # Now, the biglm() function should be in your environment.
#' # ls()
#' # biglm() now appears in the graph.
#' # vis_drake_graph(plan)
#' # And subsequent make()s respond to changes to biglm()
#' # and its dependencies.
#' })
#' }
#' # nolint end
expose_imports <- function(
  package,
  character_only = FALSE,
  envir = parent.frame(),
  jobs = 1
) {
  force(envir)
  if (!character_only) {
    package <- as.character(substitute(package))
  }
  expose_envir(from = getNamespace(package), to = envir, jobs = jobs)
}

expose_envir <- function(from, to, jobs, keep = names(from)) {
  from <- as.list(from, all.names = TRUE)[keep]
  from <- list2env(from, parent = globalenv())
  lightly_parallelize(
    X = ls(from, all.names = TRUE),
    FUN = function(name) {
      value <- get(name, envir = from)
      if (typeof(value) == "closure") {
        assign(
          x = name,
          envir = to,
          value = `environment<-`(value, from)
        )
      } else {
        assign(x = name, envir = to, value = value)
      }
    },
    jobs = jobs
  )
  to
}
