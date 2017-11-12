#' @title Function \code{analyses}
#' @description Use \code{\link{plan_analyses}()} instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @seealso \code{\link{plan_analyses}}
#' @return The same return value as \code{\link{plan_analyses}()}.
#' @param plan Same as for \code{\link{plan_analyses}()}.
#' @param datasets Same as for \code{\link{plan_analyses}()}.
#' @examples
#' # See ?plan_analyses for examples.
analyses <- function(plan, datasets){
  .Deprecated(
    "backend",
    package = "drake",
    msg = paste(
      "drake::analyses() is deprecated",
      "due to possible name conflicts.",
      "Use plan_analyses() instead."
    )
  )
  plan_analyses(plan = plan, datasets = datasets)
}

#' @title Deprecated function \code{backend}
#' @description Use \code{future::plan()} instead.
#' Avoid \code{drake::plan()}.
#' @details Deprecated on 2017-11-12.
#' @export
#' @return The same return value as \code{future::plan()}.
#' @param ... Arguments to \code{future::plan()}.
#' @examples
#' \dontrun{
#' load_basic_example() # Load the canonical example
#' # Choose future's multicore parallel backend.
#' future::plan(multicore) # Instead of backend(). Avoid drake::plan().
#' # Run the project, build the targets.
#' # Future knows that you chose the multicore backend.
#' make(my_plan, parallelism = "future_lapply")
#' }
backend <- function(...){
  .Deprecated(
    "backend",
    package = "drake",
    msg = paste(
      "drake::backend() is deprecated.",
      "Use future::plan() directly.",
      "drake::backend() only exists because of a name conflict",
      "between drake::plan() and future::plan().",
      "Once enough time has passed for users to adjust,",
      "drake::plan() will be removed."
    )
  )
  future::plan(...)
}

#' @title Deprecated function \code{check}
#' @description Use \code{\link{check_plan}()} instead.
#' @details Deprecated on 2017-11-12.
#' @seealso \code{\link{check_plan}}
#' @export
#' @return Same as for \code{\link{check_plan}()}.
#' @param plan Same as for \code{\link{check_plan}()}.
#' @param targets Same as for \code{\link{check_plan}()}.
#' @param envir Same as for \code{\link{check_plan}()}.
#' @param cache Same as for \code{\link{check_plan}()}.
#' @param verbose Same as for \code{\link{check_plan}()}.
#' @examples
#' \dontrun{
#' default_system2_args(jobs = 1, verbose = TRUE)
#' }
check <- function(
  plan = workplan(),
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  cache = drake::get_cache(verbose = verbose),
  verbose = TRUE
){
  .Deprecated(
    "check",
    package = "drake",
    msg = paste(
      "drake::check() is deprecated",
      "due to a conflict with devtools::check().",
      "Use check_plan() instead."
    )
  )
  check_plan(
    plan = plan,
    targets = targets,
    envir = envir,
    cache = cache,
    verbose = verbose
  )
}

#' @title Deprecated function \code{default_system2_args}
#' @description Use \code{\link{default_Makefile_args}()} instead.
#' @details Deprecated on 2017-10.
#' @seealso \code{\link{default_Makefile_args}}
#' @export
#' @return \code{args} for \code{\link{system2}(command, args)}
#' @param jobs number of jobs
#' @param verbose logical, whether to be verbose
#' @examples
#' \dontrun{
#' default_system2_args(jobs = 1, verbose = TRUE)
#' }
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

# Deprecated ..analysis.. and ..dataset.. on 2017-11-12
# in favor of analysis__ and dataset__
deprecate_wildcard <- function(plan, old, replacement){
  if (any(grepl(old, plan$command, fixed = TRUE))){
    warning(
      "The '", old, "' wildcard is deprecated. ",
      "Use '", replacement, "' instead.",
      call. = FALSE
    )
  }
  plan$command <- gsub(
    pattern = old,
    replacement = replacement,
    x = plan$command,
    fixed = TRUE
  )
  plan
}

#' @title Deprecated function evaluate
#' @description Use \code{\link{evaluate_plan}()} instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @seealso \code{\link{evaluate_plan}}
#' @return Same as for \code{\link{evaluate_plan}}
#' @param plan Same as for \code{\link{evaluate_plan}}
#' @param rules Same as for \code{\link{evaluate_plan}}
#' @param wildcard Same as for \code{\link{evaluate_plan}}
#' @param values Same as for \code{\link{evaluate_plan}}
#' @param expand Same as for \code{\link{evaluate_plan}}
#' @examples
#' # See ?evaluate_plan for examples.
evaluate <- function(
  plan,
  rules = NULL,
  wildcard = NULL,
  values = NULL,
  expand = TRUE
){
  .Deprecated(
    "evaluate",
    package = "drake",
    msg = paste(
      "drake::evaluate() is deprecated",
      "due to a conflict with evaluate::evaluate().",
      "Use evaluate_plan() instead."
    )
  )
  evaluate_plan(
    plan = plan,
    rules = rules,
    wildcard = wildcard,
    values = values,
    expand = expand
  )
}

#' @title Deprecated function expand
#' @description Use \code{\link{expand_plan}()} instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @seealso \code{\link{expand_plan}}
#' @return Same as for \code{\link{expand_plan}}
#' @param plan Same as for \code{\link{expand_plan}}
#' @param values Same as for \code{\link{expand_plan}}
#' @examples
#' # See ?expand_plan for examples.
expand <- function(
  plan,
  values = NULL
){
  .Deprecated(
    "expand",
    package = "drake",
    msg = paste(
      "drake::expand() is deprecated",
      "due to a conflict with tidyr::expand().",
      "Use expand_plan() instead."
    )
  )
  expand_plan(
    plan = plan,
    values = values
  )
}

#' @title Deprecated function \code{gather}
#' @description Use \code{\link{gather_plan}()} instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @seealso \code{\link{gather_plan}}
#' @return Same as for \code{\link{gather_plan}}
#' @param plan Same as for \code{\link{gather_plan}}
#' @param target Same as for \code{\link{gather_plan}}
#' @param gather Same as for \code{\link{gather_plan}}
#' @examples
#' # See ?gather_plan for examples.
gather <- function(
  plan = NULL,
  target = "target",
  gather = "list"
){
  .Deprecated(
    "gather",
    package = "drake",
    msg = paste(
      "drake::gather() is deprecated",
      "due to a conflict with tidyr::gather().",
      "Use gather_plan() instead."
    )
  )
  gather_plan(
    plan = plan,
    target = target,
    gather = gather
  )
}

#' @title Deprecated function \code{plan}
#' @description Use \code{\link{workplan}()} instead.
#' @details Deprecated on 2017-10.
#' @seealso \code{\link{workplan}}
#' @export
#' @return A data frame of targets and commands.
#' @param ... Same as for \code{\link{workplan}()}.
#' @param list Same as for \code{\link{workplan}()}.
#' @param file_targets Same as for \code{\link{workplan}()}.
#' @param strings_in_dots Same as for \code{\link{workplan}()}.
#' @examples
#' # See ?workplan for examples.
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

#' @title Deprecated function \code{session}
#' @description Use \code{\link{drake_session}()} instead
#' @details Deprecated on 2017-11-12.
#' @seealso \code{\link{drake_session}}
#' @export
#' @return Same as for \code{\link{drake_session}()}.
#' @param cache Same as for \code{\link{drake_session}()}.
#' @param path Same as for \code{\link{drake_session}()}.
#' @param search Same as for \code{\link{drake_session}()}.
#' @param verbose Same as for \code{\link{drake_session}()}.
#' @examples
#' # See ?drake_session for examples.
session <- function(
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = TRUE
){
  .Deprecated(
    "session",
    package = "drake",
    msg = paste(
      "drake::session() is deprecated",
      "due to a possible conlict with the R/shiny lexicon.",
      "Use drake_session() instead."
    )
  )
  drake_session(
    path = path,
    search = search,
    cache = cache,
    verbose = verbose
  )
}

#' @title Deprecated function \code{summaries}
#' @description Use \code{\link{summaries}()} instead
#' @details Deprecated on 2017-11-12.
#' @seealso \code{\link{summaries}}
#' @export
#' @return Same as for \code{\link{plan_summaries}()}.
#' @param plan Same as for \code{\link{plan_summaries}()}.
#' @param analyses Same as for \code{\link{plan_summaries}()}.
#' @param datasets Same as for \code{\link{plan_summaries}()}.
#' @param gather Same as for \code{\link{plan_summaries}()}.
#' @examples
#' # See ?drake_session for examples.
summaries <- function(
  plan,
  analyses,
  datasets,
  gather = rep("list", nrow(plan))
){
  .Deprecated(
    "summaries",
    package = "drake",
    msg = paste(
      "drake::summaries() is deprecated",
      "due to possible name conflicts.",
      "Use plan_summaries() instead."
    )
  )
  plan_summaries(
    plan = plan,
    analyses = analyses,
    datasets = datasets,
    gather = gather
  )
}

#' @title Function \code{workflow}
#' @description Turns a named collection of command/target pairs into
#' a workflow plan data frame for \code{\link{make}} and
#' \code{\link{check}}.
#' @details Deprecated on 2017-10
#' @export
#' @return A data frame of targets and commands.
#' @param ... same as for \code{drake::\link{workplan}()}
#' @param list same as for \code{drake::\link{workplan}()}
#' @param file_targets same as for \code{drake::\link{workplan}()}
#' @param strings_in_dots same as for \code{drake::\link{workplan}()}
#' @examples
#' # See ?workplan for examples.
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
