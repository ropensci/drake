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
#' @param parallelism character, type of parallelism to use. 
#' See \code{\link{parallelism_choices}()} for the choices
#' for this argument, and run \code{?parallelism_choices} 
#' for an explanation of each. 
#' To use parallelism at all, be sure that \code{jobs >= 2}. If 
#' \code{parallelism} is \code{"mclapply"}, drake will use 
#' \code{parallel::\link{mclapply}()} to distribute targets across
#' parallel processes wherever possible. (This is not possible on
#' Windows.) If \code{"Makefile"}, drake will write
#' and execute a Makefile to distribute targets across separate
#' R sessions. The vignettes (\code{vignette(package = "drake")})
#' show how to turn those R
#' sessions into separate jobs on a cluster. Read the vignettes
#' to learn how to take advantage of multiple nodes on a 
#' supercomputer. WARNING: the Makefile is NOT standalone.
#' Do not run outside of \code{\link{run}()} or \code{\link{make}()}.
#' For \code{parallelism == "Makefile"}, Windows users will need
#' to download and install Rtools.
#' 
#' @param jobs number of parallel processes or jobs to run.
#' Windows users should not set \code{jobs > 1} if 
#' \code{parallelism} is \code{"mclapply"} because
#' \code{\link{mclapply}()} is based on forking. Windows users
#' who use \code{parallelism == "Makefile"} will need to 
#' download and install Rtools.
#' 
#' @param packages character vector packages to load, in the order
#' they should be loaded. Defaults to \code{(.packages())}, so you
#' shouldn't usually need to set this manually. Just call
#' \code{\link{library}()} to load your packages before \code{run()}.
#' However, sometimes packages need to be strictly forced to load
#' in a certian order, especially if \code{parallelism} is 
#' \code{"Makefile"}. To do this, do not use \code{\link{library}()}
#' or \code{\link{require}()} or \code{\link{loadNamespace}()} or
#' \code{\link{attachNamespace}()} to load any libraries beforehand.
#' Just list your packages in the \code{packages} argument in the order
#' you want them to be loaded.
#' If \code{parallelism} is \code{"mclapply"}, 
#' the necessary packages
#' are loaded once before any targets are built. If \code{parallelism} is 
#' \code{"Makefile"}, the necessary packages are loaded once on 
#' initialization and then once again for each target right 
#' before that target is built.
#' 
#' @param prework character vector of lines of code to run 
#' before build time. This code can be used to 
#' load packages, set options, etc., although the packages in the
#' \code{packages} argument are loaded before any prework is done.
#' If \code{parallelism} is \code{"mclapply"}, the \code{prework}
#' is run once before any targets are built. If \code{parallelism} is 
#' \code{"Makefile"}, the prework is run once on initialization 
#' and then once again for each target right before that target is built.
#' 
#' @param prepend lines to prepend to the Makefile if \code{parallelism}
#' is \code{"Makefile"}. See the vignettes 
#' (\code{vignette(package = "drake")})
#' to learn how to use \code{prepend}
#' to take advantage of multiple nodes of a supercomputer.
#' 
#' @param command character scalar, command to call the Makefile 
#' generated for distributed computing. 
#' Only applies when \code{parallelism} is \code{"Makefile"}. 
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
run = function(plan, targets = possible_targets(plan),
  envir = parent.frame(), verbose = TRUE, 
  parallelism = parallelism_choices(), jobs = 1, 
  packages = (.packages()), prework = character(0),
  prepend = character(0), command = "make", 
  args = default_system2_args(jobs = jobs, verbose = verbose)){
  force(envir)
  parallelism = match.arg(parallelism)
  config = config(plan = plan, targets = targets, envir = envir, 
    verbose = verbose, parallelism = parallelism,
    jobs = jobs, packages = packages, prework = prework, 
    prepend = prepend, command = command, args = args)
  check_config(config)
  assert_input_files_exist(config)
  config$cache$set(key = "sessionInfo", value = sessionInfo(), 
    namespace = "session")
  get(paste0("run_", parallelism))(config)
}

#' @title Function \code{make}
#' @description Same as \code{\link{run}}. 
#' Type \code{?\link{run}} for more.
#' @export
#' @param plan same as in function \code{\link{run}()}
#' @param targets same as in function \code{\link{run}()}
#' @param envir same as in function \code{\link{run}()}
#' @param verbose same as in function \code{\link{run}()}
#' @param parallelism same as in function \code{\link{run}()}
#' @param jobs same as in function \code{\link{run}()}
#' @param packages same as in function \code{\link{run}()}
#' @param prework same as in function \code{\link{run}()}
#' @param prepend same as in function \code{\link{run}()}
#' @param command same as in function \code{\link{run}()}
#' @param args same as in function \code{\link{run}()}
make = run

next_targets = function(graph_remaining_targets){
  number_dependencies = sapply(V(graph_remaining_targets), 
    function(x) 
      adjacent_vertices(graph_remaining_targets, x, mode = "in") %>% 
        unlist %>% length)
  which(!number_dependencies) %>% names
}

prune_envir = function(next_targets, config){
  load_these = dependencies(targets = next_targets, config = config) %>% 
    Filter(f = is_not_file) %>% intersect(y = config$plan$target)
  unload_these = intersect(config$plan$target, ls(config$envir)) %>% 
    setdiff(y = load_these)
  rm(list = unload_these, envir = config$envir)
  if(length(load_these)) loadd(list = load_these, envir = config$envir)
  invisible()
}
