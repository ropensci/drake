#' @title Function \code{max_useful_jobs}
#' @description Get the maximum number of useful jobs in the next call
#' to \code{make(..., jobs = YOUR_CHOICE)}.
#' @details Any additional jobs more than \code{max_useful_jobs(...)}
#' will be superfluous, and could even slow you down for
#' \code{make(..., parallelism = 'parLapply')}. Set
#' Set the \code{imports} argument to change your assumptions about
#' how fast objects/files are imported.
#' IMPORTANT: you must be in the root directory of your project.
#' @export
#'
#' @return A numeric scalar, the maximum number of useful jobs for
#' \code{\link{make}(..., jobs = ...)}.
#'
#' @seealso \code{\link{vis_drake_graph}}, \code{\link{build_drake_graph}},
#' \code{\link{shell_file}}, \code{\link{drake_config}()}
#'
#' @param config internal configuration list of \code{\link{make}(...)},
#' produced also with \code{\link{drake_config}()}.
#'
#' @param imports Set the \code{imports} argument to change your
#' assumptions about how fast objects/files are imported.
#' Possible values:
#' \itemize{
#'  \item{'all'}{: Factor all imported files/objects into
#'    calculating the max useful number of jobs.
#'    Note: this is not appropriate for
#'    \code{make(.., parallelism = 'Makefile')} because imports
#'    are processed sequentially for the Makefile option.}
#'  \item{'files'}{: Factor all imported files into the calculation,
#'    but ignore all the other imports.}
#'  \item{'none'}{: Ignore all the imports and just focus on the max number
#'    of useful jobs for parallelizing targets.}
#' }
#'
#' @param from_scratch logical, whether to assume
#' the next \code{\link{make}()} will run from scratch
#' so that all targets are attempted.
#'
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Load drake's canonical example.
#' config <- drake_config(my_plan) # Standard drake configuration list.
#' # Look at the graph. The work proceeds column by column
#' # in parallelizable stages. The maximum number of useful jobs
#' # is determined by the number and kind of targets/imports
#' # in the columns.
#' vis_drake_graph(config = config)
#' # Should be 8 because everythign is out of date.
#' max_useful_jobs(config = config) # 8
#' # Take into account targets and imported files.
#' max_useful_jobs(config = config, imports = 'files') # 8
#' # Include imported R objects too.
#' max_useful_jobs(config = config, imports = 'all') # 9
#' # Exclude all imported objects.
#' max_useful_jobs(config = config, imports = 'none') # 8
#' config <- make(my_plan) # Run the project, build the targets.
#' vis_drake_graph(config = config) # Everything is up to date.
#' # Ignore the targets already built.
#' max_useful_jobs(config = config) # 1
#' max_useful_jobs(config = config, imports = 'files') # 1
#' # Imports are never really skipped in make().
#' max_useful_jobs(config = config, imports = 'all') # 9
#' max_useful_jobs(config = config, imports = 'none') # 0
#' # Change a function so some targets are now out of date.
#' reg2 = function(d){
#'   d$x3 = d$x^3
#'   lm(y ~ x3, data = d)
#' }
#' vis_drake_graph(config = config)
#' # We have a different number for max useful jobs.
#' max_useful_jobs(config = config) # 4
#' # By default, max_useful_jobs() takes into account which
#' # targets are out of date. To assume you are building from scratch,
#' # consider using the "always" trigger.
#' max_useful_jobs(config, from_scratch = TRUE, imports = 'files') # 8
#' max_useful_jobs(config, from_scratch = TRUE, imports = 'all') # 9
#' max_useful_jobs(config, from_scratch = TRUE, imports = 'none') # 8
#' })
#' }
max_useful_jobs <- function(
  config,
  imports = c("files", "all", "none"),
  from_scratch = FALSE
){
  nodes <- parallel_stages(config = config, from_scratch = from_scratch)
  imports <- match.arg(imports)
  if (imports == "none"){
    nodes <- nodes[!nodes$imported, ]
  } else if (imports == "files"){
    nodes <- nodes[!nodes$imported | nodes$file, ]
  }
  if (!nrow(nodes)){
    return(0)
  }
  dlply(nodes, "stage", nrow) %>%
    unlist %>%
    max
}
