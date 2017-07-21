#' @title Function \code{parallelism_choices}
#' @description List the types of supported parallel computing.
#' @export
#' @return Character vector listing the types of parallel
#' computing supported.
#' @details Run \code{make(..., parallelism = x, jobs = n)} for any of
#' the following values of \code{x} to distribute targets over parallel
#' units of execution.
#' \describe{
#'  \item{"parLapply"}{launches multiple processes in a single R session
#'  using \code{parallel::\link{parLapply}()}.
#'  This is single-node, (potentially) multicore computing.
#'  It requires more overhead than the \code{"mclapply"} option,
#'  but it works on Windows. If \code{jobs} is \code{1} in 
#'  \code{\link{make}()}, then no "cluster" is created and 
#'  no parallelism is used.}
#'  \item{"mclapply"}{uses multiple processes in a single R session.
#'  This is single-node, (potentially) multicore computing.
#'  Does not work on Windows for \code{jobs > 1}
#'  because \code{\link{mclapply}()} is based on forking.}
#'  \item{"Makefile"}{uses multiple R sessions
#'  by creating and running a Makefile.
#'  The Makefile is NOT standalone.
#'  DO NOT run outside of \code{\link{make}()} or \code{\link{make}()}.
#'  Windows users will need to download and intall Rtools.
#'  As explained in the vignettes, you can use the \code{prepend}
#'  to \code{\link{make}()} or \code{\link{make}()} to distribute
#'  targets over multiple nodes of a supercomputer. Use this
#'  approach for true distributed computing.}}
#' @examples
#' parallelism_choices()
parallelism_choices = function(){
  c("parLapply", "mclapply", "Makefile")
}

#' @title Function \code{default_parallelism}
#' @description Default parallelism for \code{\link{make}()}:
#' \code{"parLapply"} for Windows machines and \code{"mclapply"}
#' for other platforms.
#' @export
#' @return default parallelism option for the current platform
#' @examples
#' default_parallelism()
default_parallelism = function(){
  ifelse(Sys.info()['sysname'] == "Windows", "parLapply", "mclapply") %>%
    unname
}

#' @title Function \code{max_useful_jobs}
#' @description Get the maximum number of useful jobs in the next call 
#' to \code{make(..., jobs = YOUR_CHOICE)}. 
#' @details Any additional jobs more than \code{max_useful_jobs(...)}
#' will be superfluous, and could even slow you down for
#' \code{make(..., parallelism = "parLapply")}. Set 
#' Set the \code{imports} argument to change your assumptions about 
#' how fast objects/files are imported.
#' IMPORTANT: you must be in the root directory of your project.
#' @export
#' @return a list of three data frames: one for nodes, one for edges, and one for
#' the legend/key nodes.
#' @seealso \code{\link{plot_graph}}, \code{\link{build_graph}}
#' @param plan workflow plan data frame, same as for function 
#' \code{\link{make}()}.
#' @param targets names of targets to bulid, same as for function
#' \code{\link{make}()}.
#' @param envir environment to import from, same as for function
#' \code{\link{make}()}.
#' @param verbose logical, whether to output messages to the console.
#' @param graph an igraph object if one has already been built with 
#' \code{\link{build_graph}()}. 
#' @param imports Set the \code{imports} argument to change your assumptions about 
#' how fast objects/files are imported. Possible values:
#' \itemize{
#'  \item{"all"}{: Factor all imported files/objects into calculating the max useful
#'    number of jobs. Note: this is not appropriate for 
#'    \code{make(.., parallelism = "Makefile")} because imports 
#'    are processed sequentially for the Makefile option.}
#'  \item{"files"}{: Factor all imported files into the calculation, but ignore
#'    all the other imports.}
#'  \item{"none"}{: Ignore all the imports and just focus on the max number
#'    of useful jobs for parallelizing targets.}
#' } 
#' @examples
#' \dontrun{
#' load_basic_example()
#' plot_graph(my_plan) # Look at the graph to make sense of the output.
#' max_useful_jobs(my_plan) # 8
#' max_useful_jobs(my_plan, imports = "files") # 8
#' max_useful_jobs(my_plan, imports = "all") # 10
#' max_useful_jobs(my_plan, imports = "none") # 8
#' make(my_plan)
#' plot_graph(my_plan)
#' # Ignore the targets already built.
#' max_useful_jobs(my_plan) # 1
#' max_useful_jobs(my_plan, imports = "files") # 1
#' max_useful_jobs(my_plan, imports = "all") # 10
#' max_useful_jobs(my_plan, imports = "none") # 0
#' # Change a function so some targets are now out of date.
#' reg2 = function(d){
#'   d$x3 = d$x^3
#'   lm(y ~ x3, data = d)
#' }
#' plot_graph(my_plan)
#' max_useful_jobs(my_plan) # 4
#' max_useful_jobs(my_plan, imports = "files") # 4
#' max_useful_jobs(my_plan, imports = "all") # 10
#' max_useful_jobs(my_plan, imports = "none") # 4
#' }
max_useful_jobs = function(plan, targets = drake::possible_targets(plan), 
  envir = parent.frame(), verbose = FALSE, graph = NULL,
  imports = c("files", "all", "none")){
  force(envir)
  nodes = dataframes_graph(plan = plan, targets = targets,
    envir = envir, verbose = verbose, graph = graph)$nodes
  imports = match.arg(imports)
  just_targets = intersect(nodes$id, plan$target)
  just_files = Filter(x = nodes$id, f = is_file)
  targets_and_files = union(just_targets, just_files)
  if(imports == "none")
    nodes = nodes[just_targets,]
  else if(imports == "files")
    nodes = nodes[targets_and_files,]
  nodes = nodes[nodes$status != "up-to-date",]
  if(!nrow(nodes)) return(0)
  dlply(nodes, "level", nrow) %>% unlist %>% max
}

run_parallel = function(config, worker){
  graph_remaining_targets = config$graph
  while(length(V(graph_remaining_targets)))
    graph_remaining_targets = parallel_stage(
      graph_remaining_targets = graph_remaining_targets,
      worker = worker, config = config)
}

parallel_stage = function(graph_remaining_targets, worker, 
  config){
  config = inventory(config)
  candidates = next_targets(graph_remaining_targets)
  hash_list = hash_list(targets = candidates, config = config)
  build_these = Filter(candidates, f = function(target)
    should_build(target = target, hash_list = hash_list,
      config = config))
  hash_list = hash_list[build_these]
  if(length(build_these))
    worker(targets = build_these, hash_list = hash_list, config = config)
  delete_vertices(graph_remaining_targets, v = candidates)
}
