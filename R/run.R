#' @title Function \code{run}
#' @description Run your project.
#' @export
#' @param plan workflow plan data frame. 
#' A workflow plan data frame is a data frame
#' with a \code{target} column and a \code{command} column.
#' Targets are the objects and files that drake generates,
#' and commands are the pieces of R code that produce them.
#' Use the function \code{\link{plan}()} to generate workflow plan
#' data frames easily, and see functions \code{\link{analyses}()},
#' \code{\link{summaries}()}, \code{\link{evaluate}()},  
#' \code{\link{expand}()}, and \code{\link{gather}()} for 
#' easy ways to generate large workflow plan data frames.
#'  
#' @param targets character string, names of targets to build.
#' Dependencies are built too. 
#' 
#' @param envir environment to use. Defaults to the current
#' workspace, so you should not need to worry about this
#' most of the time. A deep copy of \code{envir} is made, 
#' so you don't need to worry about your workspace being modified
#' by \code{run}. The deep copy inherits from the global environment.
#' Wherever necessary, objects and functions are imported
#' from \code{envir} and the global environment and
#' then reproducibly tracked as dependencies.
#' 
#' @param verbose logical, whether to print progress to the console.
#' Skipped objects are not printed.
#' 
#' @param parallelism character, type of parallelism to use. To 
#' use parallelism at all, be sure that \code{jobs >= 2}. If 
#' \code{parallelism} is \code{"single-session"}, drake will use 
#' \code{parallel::\link{mclapply}()} to distribute targets across
#' parallel processes wherever possible. (This is not possible on
#' Windows.) If \code{"distributed"}, drake will write
#' and execute a Makefile to distribute targets across separate
#' R sessions. The vignettes (\code{vignette(package = "drake")})
#' show how to turn those R
#' sessions into separate jobs on a cluster. Read the vignettes
#' to learn how to take advantage of multiple nodes on a 
#' supercomputer.
#' 
#' @param jobs number of parallel processes or jobs to run.
#' Windows users should not set \code{jobs > 1} if 
#' \code{parallelism} is \code{"single-session"} because
#' single-session parallelism is based on 
#' \code{parallel::\link{mclapply}()}
#' 
#' @param packages character vector packages to load, in the order
#' they should be loaded. Defaults to \code{(.packages())}, so you
#' shouldn't usually need to set this manually. Just call
#' \code{\link{library}()} to load your packages before \code{run()}.
#' However, sometimes packages need to be strictly forced to load
#' in a certian order, especially if \code{parallelism} is 
#' \code{"distributed"}. To do this, do not use \code{\link{library}()}
#' or \code{\link{require}()} or \code{\link{loadNamespace}()} or
#' \code{\link{attachNamespace}()} to load any libraries beforehand.
#' Just list your packages in the \code{packages} argument in the order
#' you want them to be loaded.
#' If \code{parallelism} is \code{"single-session"}, 
#' the necessary packages
#' are loaded once before any targets are built. If \code{parallelism} is 
#' \code{"distributed"}, the necessary packages are loaded once on 
#' initialization and then once again for each target right 
#' before that target is built.
#' 
#' @param prework character vector of lines of code to run 
#' before build time. This code can be used to 
#' load packages, set options, etc., although the packages in the
#' \code{packages} argument are loaded before any prework is done.
#' If \code{parallelism} is \code{"single-session"}, the \code{prework}
#' is run once before any targets are built. If \code{parallelism} is 
#' \code{"distributed"}, the prework is run once on initialization 
#' and then once again for each target right before that target is built.
#' 
#' @param prepend lines to prepend to the Makefile if \code{parallelism}
#' is \code{"distributed"}. See the vignettes 
#' (\code{vignette(package = "drake")})
#' to learn how to use \code{prepend}
#' to take advantage of multiple nodes of a supercomputer.
#' 
#' @param command character scalar, command to call the Makefile 
#' generated for distributed computing. 
#' Only applies when \code{parallelism} is \code{"distributed"}. 
#' Defaults to the usual \code{"make"}, but it could also be 
#' \code{"lsmake"} on supporting systems, for example. 
#' \code{command} and \code{args} are executed via 
#' \code{\link{system2}(command, args)} to run the Makefile.
#' If \code{args} has something like \code{"--jobs=2"}, or if 
#' \code{jobs >= 2} and \code{args} is left alone, targets
#' will be distributed over independent parallel R sessions
#' wherever possible.
#' 
#' @param args command line arguments to call the Makefile for
#' distributed computing. For advanced users only. If set,
#' \code{jobs} and \code{verbose} are overwritten as they apply to the 
#' Makefile.
#' \code{command} and \code{args} are executed via 
#' \code{\link{system2}(command, args)} to run the Makefile.
#' If \code{args} has something like \code{"--jobs=2"}, or if 
#' \code{jobs >= 2} and \code{args} is left alone, targets
#' will be distributed over independent parallel R sessions
#' wherever possible.
run = function(plan, targets = plan$target, envir = parent.frame(), 
  verbose = TRUE, parallelism = c("single-session", "distributed"), 
  jobs = 1, packages = (.packages()), prework = character(0),
  prepend = character(0), command = "make", 
  args = default_system2_args(jobs = jobs, verbose = verbose)){
  force(envir)
  parallelism = match.arg(parallelism)
  prework = add_packages_to_prework(packages = packages, 
    prework = prework)
  args = setup(plan = plan, targets = targets, envir = envir, 
    verbose = verbose, jobs = jobs, prework = prework,
    command = command, args = args)
  check_args(args)
  args$cache$set(key = "sessionInfo", value = sessionInfo(), 
    namespace = "session")
  if(parallelism == "single-session")
    run_mclapply(args)
  else if(parallelism == "distributed")
    run_makefile(args)
}

add_packages_to_prework = function(packages, prework){
  if(!length(packages)) return(prework)
  package_list = deparse(packages) %>% paste(collapse = "\n")
  paste0("drake::load_if_missing(", package_list, ")") %>%
    c(prework)
}

#' @title Function \code{load_if_missing}
#' @description loads and attaches packages 
#' if they are not already loaded.
#' @export
#' @param packages character vector of package names.
load_if_missing = function(packages){
  Filter(packages, f = isPackageLoaded) %>%
    setdiff(x = packages) %>% 
    lapply(FUN = require, character.only = TRUE) %>%
    invisible
}

setup = function(plan, targets, envir, jobs, verbose, prework,
  command, args){
  plan = fix_deprecated_plan_names(plan)
  targets = intersect(targets, plan$target)
  cache = storr_rds(cachepath, mangle_key = TRUE)
  cache$clear(namespace = "status")
  envir = envir %>% as.list %>% list2env(parent = globalenv())
  graph = build_graph(plan = plan, targets = targets,
    envir = envir)
  order = topological.sort(graph)$name
  list(plan = plan, targets = targets, envir = envir, cache = cache, 
    jobs = jobs, verbose = verbose, prework = prework,
    command = command, args = args, graph = graph, order = order)
}

run_mclapply = function(args){
  evals(args$prework, .with = args$envir)
  next_graph = args$graph
  while(length(V(next_graph))) 
    next_graph = parallel_stage(next_graph = next_graph, args = args)
}

next_targets = function(next_graph){
  number_dependencies = sapply(V(next_graph), 
    function(x) 
      adjacent_vertices(next_graph, x, mode = "in") %>% 
        unlist %>% length)
  which(!number_dependencies) %>% names
}

parallel_stage = function(next_graph, args){
  next_targets = next_targets(next_graph) 
  prune_envir(next_targets = next_targets, args = args)
  mclapply(next_targets, build, mc.cores = args$jobs, args = args)
  delete_vertices(next_graph, v = next_targets)
}

prune_envir = function(next_targets, args){
  load_these = dependencies(targets = next_targets, args = args) %>% 
    Filter(f = is_not_file) %>% intersect(y = args$plan$target)
  unload_these = intersect(args$plan$target, ls(args$envir)) %>% 
    setdiff(y = load_these)
  rm(list = unload_these, envir = args$envir)
  if(length(load_these)) loadd(list = load_these, envir = args$envir)
  invisible()
}
