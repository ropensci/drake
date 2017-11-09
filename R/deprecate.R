fix_deprecated_plan_names <- function(plan){
  if (any(colnames(plan) %in% c("output", "code"))){
    warning("Drake is no longer using \"output\" or \"code\" ",
      "for column names in workflow plan data frames. Use \"target\" ",
      "and \"command\" instead.",
      call. = FALSE
    )
  }
  colnames(plan) <- gsub("^output$", "target", colnames(plan)) %>%
    gsub(pattern = "^code$", replacement = "command")
  as.data.frame(plan, stringsAsFactors = FALSE)
}

#' @title Deprecated function \code{plan}
#' @description Turns a named collection of command/target pairs into
#' a workflow plan data frame for \code{\link{make}} and
#' \code{\link{check}}. Deprecated in favor of
#' \code{\link{workplan}()} due to a name conflict with
#' \code{future::plan()}.
#' @seealso \code{\link{workplan}}, \code{\link{make}},
#' \code{\link{check}}
#' @export
#' @return data frame of targets and command
#' @param ... commands named by the targets they generate.
#' Recall that drake uses single quotes to denote external files
#' and double-quoted strings as ordinary strings.
#' Use the \code{strings_in_dots} argument to control the
#' quoting in \code{...}.
#' @param list character vector of commands named
#' by the targets they generate.
#' @param file_targets logical. If \code{TRUE}, targets are single-quoted
#' to tell drake that these are external files that should be expected
#' as output in the next call to \code{\link{make}()}.
#' @param strings_in_dots character scalar. If \code{"filenames"},
#' all character strings in \code{...} will be treated as names of file
#' dependencies (single-quoted). If \code{"literals"}, all
#' character strings in \code{...} will be treated as ordinary
#' strings, not dependencies of any sort (double-quoted).
#' Because of R's automatic parsing/deparsing behavior,
#' strings in \code{...} cannot simply be left alone.
#' @examples
#' # plan() is deprecated. Use workplan() instead.
#' workplan(small = simulate(5), large = simulate(50))
#' workplan(list = c(x = "1 + 1", y = "sqrt(x)"))
#' workplan(data = readRDS("my_data.rds"))
#' workplan(
#'   my_file.rds = saveRDS(1+1, "my_file.rds"),
#'   file_targets = TRUE,
#'   strings_in_dots = "literals"
#' )
plan <- function(
  ...,
  list = character(0),
  file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")
){
  .Deprecated(
    "plan",
    package = "drake",
    msg = paste(
      "drake::plan() is deprecated due to a",
      "conflict with future::plan().",
      "Use workplan() instead."
    )
  )
  strings_in_dots <- match.arg(strings_in_dots)
  dots <- match.call(expand.dots = FALSE)$...
  commands_dots <- lapply(dots, wide_deparse)
  names(commands_dots) <- names(dots)
  commands <- c(commands_dots, list)
  targets <- names(commands)
  commands <- as.character(commands)
  if (!length(commands)){
    return(
      data.frame(
        target = character(0),
        command = character(0)
      )
    )
  }
  plan <- data.frame(
    target = targets,
    command = commands,
    stringsAsFactors = FALSE
  )
  from_dots <- plan$target %in% names(commands_dots)
  if (file_targets){
    plan$target <- eply::quotes(plan$target, single = TRUE)
  }
  if (strings_in_dots == "filenames"){
    plan$command[from_dots] <- gsub("\"", "'", plan$command[from_dots])
  }
  sanitize_plan(plan)
}

#' @title Function \code{workflow}
#' @description Turns a named collection of command/target pairs into
#' a workflow plan data frame for \code{\link{make}} and
#' \code{\link{check}}.
#' @details A workflow plan data frame is a data frame
#' with a \code{target} column and a \code{command} column.
#' Targets are the objects and files that drake generates,
#' and commands are the pieces of R code that produce them.
#'
#' For file inputs and targets, drake uses single quotes.
#' Double quotes are reserved for ordinary strings.
#' The distinction is important because drake thinks about
#' how files, objects, targets, etc. depend on each other.
#' Quotes in the \code{list} argument are left alone,
#' but R messes with quotes when it parses the free-form
#' arguments in \code{...}, so use the \code{strings_in_dots}
#' argument to control the quoting in \code{...}.
#' @export
#' @return data frame of targets and command
#' @param ... same as for \code{drake::\link{workplan}()}
#' @param list same as for \code{drake::\link{workplan}()}
#' @param file_targets same as for \code{drake::\link{workplan}()}
#' @param strings_in_dots same as for \code{drake::\link{workplan}()}
#' @examples
#' # workflow() is deprecated. Use workplan() instead.
#' workplan(small = simulate(5), large = simulate(50))
#' workplan(list = c(x = "1 + 1", y = "sqrt(x)"))
#' workplan(data = readRDS("my_data.rds"))
#' workplan(my_file.rds = saveRDS(1+1, "my_file.rds"), file_targets = TRUE,
#'   strings_in_dots = "literals")
workflow <- function(
  ...,
  list = character(0),
  file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")
){
  .Deprecated(
    "workflow",
    package = "drake",
    msg = "workflow() is deprecated. Use workplan() instead."
  )
  strings_in_dots <- match.arg(strings_in_dots)
  dots <- match.call(expand.dots = FALSE)$...
  commands_dots <- lapply(dots, wide_deparse)
  names(commands_dots) <- names(dots)
  commands <- c(commands_dots, list)
  targets <- names(commands)
  commands <- as.character(commands)
  if (!length(commands)){
    return(
      data.frame(
        target = character(0),
        command = character(0)
      )
    )
  }
  plan <- data.frame(
    target = targets,
    command = commands,
    stringsAsFactors = FALSE
  )
  from_dots <- plan$target %in% names(commands_dots)
  if (file_targets){
    plan$target <- eply::quotes(plan$target, single = TRUE)
  }
  if (strings_in_dots == "filenames"){
    plan$command[from_dots] <- gsub("\"", "'", plan$command[from_dots])
  }
  sanitize_plan(plan)
}

#' @title Deprecated function \code{prune}
#' @description Use \code{\link{clean}()} instead
#' @seealso \code{\link{clean}}, \code{\link{make}}
#' @export
#' @param plan workflow plan data frame, as generated by
#' \code{\link{workplan}}.
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' cached()
#' prune(my_plan[1:3,])
#' cached()
#' make(my_plan)
#' clean(destroy = TRUE)
#' }
prune <- function(plan = workplan()){
  .Deprecated(
    "clean",
    package = "drake",
    msg = "prune() is deprecated. Use clean() instead."
  )
  clean(list = setdiff(built(), possible_targets(plan)))
}

#' @title Deprecated function \code{status}
#' @description Use \code{\link{progress}()} instead.
#' Gets the build progress (overall or individual targets)
#' of the last call to \code{\link{make}()}.
#' Objects that drake imported, built, or attempted
#' to build are listed as \code{"finished"} or \code{"in progress"}.
#' Skipped objects are not listed.
#' @seealso \code{\link{progress}},
#' \code{\link{built}}, \code{\link{imported}},
#' \code{\link{readd}}, \code{\link{workplan}}, \code{\link{make}}
#' @export
#' @return Either the build progress of each target given (from the last
#' call to \code{\link{make}()} or \code{\link{make}()}), or if no
#' targets are specified, a data frame containing the build progress
#' of the last session.
#' In the latter case, only finished targets are listed.
#' @return Either a named logical indicating whether the given
#' targets or cached or a character vector listing all cached
#' items, depending on whether any targets are specified
#' @param ... objects to load from the cache, as names (unquoted)
#' or character strings (quoted). Similar to \code{...} in
#' \code{\link{remove}(...)}.
#' @param list character vector naming objects to be loaded from the
#' cache. Similar to the \code{list} argument of \code{\link{remove}()}.
#' @param no_imported_objects logical, whether to only return information
#' about imported files and targets with commands (i.e. whether to ignore
#' imported objects that are not files).
#' @param imported_files_only logical, deprecated.
#' Same as \code{no_imported_objects}.
#' Use the \code{no_imported_objects} argument instead.
#' @param path Root directory of the drake project,
#' or if \code{search} is \code{TRUE}, either the
#' project root or a subdirectory of the project.
#' @param search If \code{TRUE}, search parent directories
#' to find the nearest drake cache. Otherwise, look in the
#' current working directory only.
#' @examples
#' \dontrun{
#' load_basic_example()
#' make(my_plan)
#' status() # Deprecated. Use progress() instead.
#' status(small, large)
#' status(list = c("small", "large"))
#' status(no_imported_objects = TRUE)
#' }
status <- function(
  ...,
  list = character(0),
  no_imported_objects = FALSE,
  imported_files_only = logical(0),
  path = getwd(),
  search = TRUE
){
  .Deprecated(
    "progress",
    package = "drake",
    msg = "status() is deprecated. Use progress() instead."
  )
  progress(
    ...,
    list = list,
    no_imported_objects = no_imported_objects,
    imported_files_only = imported_files_only,
    path = path,
    search = search
  )
}

#' @title Deprecated function \code{default_system2_args}
#' @description Use \code{\link{default_Makefile_args}()} instead.
#' @seealso \code{\link{default_Makefile_args}}
#' @export
#' @return \code{args} for \code{\link{system2}(command, args)}
#' @param jobs number of jobs
#' @param verbose logical, whether to be verbose
default_system2_args <- function(jobs, verbose){
  .Deprecated(
    "default_system2_args",
    package = "drake",
    msg = paste(
      "default_system2_args() is deprecated.",
      "Use default_Makefile_args() instead."
    )
  )
  out <- paste0("--jobs=", jobs)
  if (!verbose){
    out <- c(out, "--silent")
  }
  return(out)
}
