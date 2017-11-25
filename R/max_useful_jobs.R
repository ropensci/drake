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
#' \code{\link{shell_file}}
#'
#' @param plan workflow plan data frame, same as for function
#' \code{\link{make}()}.
#'
#' @param from_scratch logical, whether to compute the max
#' useful jobs as if the workplan were to run from scratch
#' (with all targets out of date).
#'
#' @param targets names of targets to build, same as for function
#' \code{\link{make}()}.
#'
#' @param envir environment to import from, same as for function
#' \code{\link{make}()}. \code{config$envir} is ignored in favor of
#' \code{envir}.
#'
#' @param verbose same as for \code{\link{make}()}
#'
#' @param hook same as for \code{\link{make}}
#'
#' @param cache optional drake cache. See code{\link{new_cache}()}. If
#' The \code{cache} argument is ignored if a non-null
#' \code{config} argument is supplied.
#'
#' @param jobs The \code{outdated()} function is called internally,
#' and it needs to import objects and examine your
#' input files to see what has been updated. This could take some time,
#' and parallel computing may be needed
#' to speed up the process. The \code{jobs} argument is number of parallel jobs
#' to use for faster computation.
#'
#' @param parallelism Choice of parallel backend to speed up the computation.
#' Execution order in \code{\link{make}()} is slightly different
#' when \code{parallelism} equals \code{'Makefile'}
#' because in that case, all the imports are
#' imported before any target is built.
#' Thus, \code{max_useful_jobs()} may give a
#' different answer for Makefile parallelism.
#' See \code{?parallelism_choices} for details.
#'
#' @param packages same as for \code{\link{make}}
#'
#' @param prework same as for \code{\link{make}}
#'
#' @param config internal configuration list of \code{\link{make}(...)},
#' produced also with \code{\link{config}()}.
#' \code{config$envir} is ignored.
#' Otherwise, if not \code{NULL}, \code{config}
#' overrides all the other arguments except
#' \code{imports} and \code{from_scratch}.
#' For example,
#' \code{plan} is replaced with \code{config$plan}.
#' Computing \code{\link{config}}
#' in advance could save time if you plan multiple calls to
#' \code{dataframes_graph()}.
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
#' @param make_imports logical, whether to import external files
#' and objects from the user's workspace to determine
#' which targets are up to date. If \code{FALSE}, the computation
#' is faster, but all the relevant information is drawn from the cache
#' and may be out of date.
#'
#' @examples
#' \dontrun{
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
#' config_from_scratch <- config
#' config_from_scratch$trigger <- "always"
#' max_useful_jobs(config = config_from_scratch, imports = 'files') # 8
#' max_useful_jobs(config = config_from_scratch, imports = 'all') # 9
#' max_useful_jobs(config = config_from_scratch, imports = 'none') # 8
#' }
max_useful_jobs <- function(
  config,
  imports = c("files", "all", "none")
){
  nodes <- real_stages(config = config)
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
