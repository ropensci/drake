#' @title Deprecated function \code{analyses}
#' @description Use \code{\link{plan_analyses}()} instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
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

#' @title Deprecated function \code{as_file}
#' @description Use \code{\link{as_drake_filename}()} instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @seealso \code{\link{as_drake_filename}}
#' @return The same return value as \code{\link{as_drake_filename}()}.
#' @param x Same as for \code{\link{as_drake_filename}()}.
#' @examples
#' # See ?as_drake_filename for examples.
as_file <- function(x){
  .Deprecated(
    "backend",
    package = "drake",
    msg = paste(
      "drake::as_file() is deprecated",
      "due to possible name conflicts.",
      "Use as_drake_filename() instead."
    )
  )
  as_drake_filename(x)
}

#' @title Deprecated function \code{backend}
#' @description Use \code{future::plan()} instead.
#' Avoid \code{drake::plan()}.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @return The same return value as \code{future::plan()}.
#' @param ... Arguments to \code{future::plan()}.
#' @examples
#' \dontrun{
#' load_basic_example() # Load the canonical example
#' # Choose future's multicore parallel backend.
#' library(future)
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

#' @title Deprecated function \code{build_graph}
#' @description Use \code{\link{build_drake_graph}()} instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @seealso \code{\link{build_drake_graph}}
#' @return The same return value as \code{\link{build_drake_graph}()}.
#' @param plan Same as for \code{\link{build_drake_graph}()}.
#' @param targets Same as for \code{\link{build_drake_graph}()}.
#' @param envir Same as for \code{\link{build_drake_graph}()}.
#' @param verbose Same as for \code{\link{build_drake_graph}()}.
#' @param jobs Same as for \code{\link{build_drake_graph}()}.
#' @examples
#' # See ?as_drake_filename for examples.
build_graph <- function(
  plan = plan_drake(),
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  verbose = 1,
  jobs = 1
){
  .Deprecated(
    "backend",
    package = "drake",
    msg = paste(
      "drake::build_graph() is deprecated",
      "due to possible name conflicts.",
      "Use build_drake_graph() instead."
    )
  )
  build_drake_graph(
    plan = plan, targets = targets, envir = envir, verbose = verbose,
    jobs = jobs
  )
}

#' @title Deprecated function \code{check}
#' @description Use \code{\link{check_plan}()} instead.
#' @details Deprecated on 2017-11-12.
#' @seealso \code{\link{check_plan}}
#' @export
#' @keywords internal
#' @return Same as for \code{\link{check_plan}()}.
#' @param plan Same as for \code{\link{check_plan}()}.
#' @param targets Same as for \code{\link{check_plan}()}.
#' @param envir Same as for \code{\link{check_plan}()}.
#' @param cache Same as for \code{\link{check_plan}()}.
#' @param verbose Same as for \code{\link{check_plan}()}.
#' @examples
#' # See ?check_plan for examples.
check <- function(
  plan = plan_drake(),
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

#' @title Deprecated function config
#' @description Use \code{\link{drake_config}()} instead.
#' @details Deprecated on 2017-11-12.
#' @seealso \code{\link{drake_config}}
#' @export
#' @keywords internal
#' @return The master internal configuration list of a project.
#' @seealso \code{\link{drake_config}}
#' @param plan same as for \code{\link{make}}
#' @param targets same as for \code{\link{make}}
#' @param envir same as for \code{\link{make}}
#' @param verbose same as for \code{\link{make}}
#' @param hook same as for \code{\link{make}}
#' @param parallelism same as for \code{\link{make}}
#' @param jobs same as for \code{\link{make}}
#' @param packages same as for \code{\link{make}}
#' @param prework same as for \code{\link{make}}
#' @param prepend same as for \code{\link{make}}
#' @param command same as for \code{\link{make}}
#' @param args same as for \code{\link{make}}
#' @param recipe_command same as for \code{\link{make}}
#' @param cache same as for \code{\link{make}}
#' @param timeout same as for \code{\link{make}}
#' @param cpu same as for \code{\link{make}}
#' @param elapsed same as for \code{\link{make}}
#' @param retries same as for \code{\link{make}}
#' @param force same as for \code{\link{make}}
#' @param clear_progress same as for \code{\link{drake_config}}
#' @param graph same as for \code{\link{drake_config}}
#' @param trigger same as for \code{\link{make}}
#' @param skip_imports same as for \code{\link{drake_config}}
#' @examples
#' # See ?drake_config for the examples.
config <- function(
  plan = plan_drake(),
  targets = drake::possible_targets(plan),
  envir = parent.frame(),
  verbose = 1,
  hook = default_hook,
  cache = drake::get_cache(verbose = verbose, force = force),
  parallelism = drake::default_parallelism(),
  jobs = 1,
  packages = rev(.packages()),
  prework = character(0),
  prepend = character(0),
  command = drake::default_Makefile_command(),
  args = drake::default_Makefile_args(
    jobs = jobs,
    verbose = verbose
  ),
  recipe_command = drake::default_recipe_command(),
  timeout = Inf,
  cpu = timeout,
  elapsed = timeout,
  retries = 0,
  force = FALSE,
  clear_progress = FALSE,
  graph = NULL,
  trigger = "any",
  skip_imports = FALSE
){
  .Deprecated(
    "evaluate",
    package = "drake",
    msg = paste(
      "drake::evaluate() is deprecated",
      "due to possible name conflicts.",
      "Use drake_config() instead."
    )
  )
  drake_config(
    plan = plan,
    targets = targets,
    envir = envir,
    verbose = verbose,
    hook = hook,
    cache = cache,
    parallelism = parallelism,
    jobs = jobs,
    packages = packages,
    prework = prework,
    prepend = prepend,
    command = command,
    args = args,
    recipe_command = recipe_command,
    timeout = timeout,
    cpu = cpu,
    elapsed = elapsed,
    retries = retries,
    force = force,
    clear_progress = clear_progress,
    graph = graph,
    trigger = trigger,
    skip_imports = skip_imports
  )
}

#' @title Deprecated function \code{default_system2_args}
#' @description Use \code{\link{default_Makefile_args}()} instead.
#' @details Deprecated on 2017-10.
#' @seealso \code{\link{default_Makefile_args}}
#' @export
#' @keywords internal
#' @return \code{args} for \code{\link{system2}(command, args)}
#' @param jobs number of jobs
#' @param verbose logical, whether to be verbose
#' @examples
#' # See ?default_Makefile_args for examples.
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
#' @keywords internal
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

#' @title Function \code{example_drake}
#' @description Use \code{\link{drake_example}()} instead.
#' @details Deprecated on 2017-11-12.
#' @seealso \code{\link{drake_example}}
#' @export
#' @keywords internal
#' @return \code{NULL}
#' @param example Same as for \code{\link{drake_example}()}
#' @param destination Same as for \code{\link{drake_example}()}
#' @examples
#' # See ?drake_example for examples.
example_drake <- function(
  example = drake::drake_examples(),
  destination = getwd()
){
  .Deprecated(
    "example_drake",
    package = "drake",
    msg = paste(
      "drake::example_drake() is deprecated.",
      "Use drake_example() instead."
    )
  )
  drake_example(
    example = example,
    destination = destination
  )
}

#' @title Function \code{examples_drake}
#' @description Use \code{\link{drake_examples}()} instead.
#' @details Deprecated on 2017-11-12.
#' @seealso \code{\link{drake_examples}}
#' @export
#' @keywords internal
#' @return Names of all the drake examples.
#' @examples
#' # See ?drake_examples for the examples.
examples_drake <- function() {
  .Deprecated(
    "example_drake",
    package = "drake",
    msg = paste(
      "drake::example_drake() is deprecated.",
      "Use drake_example() instead."
    )
  )
  drake_examples()
}

#' @title Deprecated function expand
#' @description Use \code{\link{expand_plan}()} instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
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
#' @keywords internal
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
#' @description Use \code{\link{plan_drake}()} instead.
#' @details Deprecated on 2017-10.
#' @seealso \code{\link{plan_drake}}
#' @export
#' @keywords internal
#' @return A data frame of targets and commands.
#' @param ... Same as for \code{\link{plan_drake}()}.
#' @param list Same as for \code{\link{plan_drake}()}.
#' @param file_targets Same as for \code{\link{plan_drake}()}.
#' @param strings_in_dots Same as for \code{\link{plan_drake}()}.
#' @examples
#' # See ?plan_drake for examples.
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
      "Use plan_drake() instead."
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
    plan$target <- drake::drake_quotes(plan$target, single = TRUE)
  }
  if (strings_in_dots == "filenames"){
    plan$command[from_dots] <- gsub("\"", "'", plan$command[from_dots])
  }
  sanitize_plan(plan)
}

#' @title Deprecated function \code{plot_graph}
#' @description Use \code{\link{vis_drake_graph}()} instead.
#' @details Deprecated on 2017-10.
#' @seealso \code{\link{vis_drake_graph}}
#' @export
#' @keywords internal
#' @return Same as for \code{\link{vis_drake_graph}()}.
#' @param plan Same as for \code{\link{vis_drake_graph}()}.
#' @param envir Same as for \code{\link{vis_drake_graph}()}.
#' @param hook Same as for \code{\link{vis_drake_graph}()}.
#' @param cache Same as for \code{\link{vis_drake_graph}()}.
#' @param jobs Same as for \code{\link{vis_drake_graph}()}.
#' @param packages Same as for \code{\link{vis_drake_graph}()}.
#' @param prework Same as for \code{\link{vis_drake_graph}()}.
#' @param verbose Same as for \code{\link{vis_drake_graph}()}.
#' @param config Same as for \code{\link{vis_drake_graph}()}.
#' @param file Same as for \code{\link{vis_drake_graph}()}.
#' @param build_times Same as for \code{\link{vis_drake_graph}()}.
#' @param digits Same as for \code{\link{vis_drake_graph}()}.
#' @param targets_only Same as for \code{\link{vis_drake_graph}()}.
#' @param split_columns Same as for \code{\link{vis_drake_graph}()}.
#' @param font_size Same as for \code{\link{vis_drake_graph}()}.
#' @param layout Same as for \code{\link{vis_drake_graph}()}.
#' @param main Same as for \code{\link{vis_drake_graph}()}.
#' @param direction Same as for \code{\link{vis_drake_graph}()}.
#' @param hover Same as for \code{\link{vis_drake_graph}()}.
#' @param navigationButtons Same as for \code{\link{vis_drake_graph}()}. # nolint
#' @param from Same as for \code{\link{vis_drake_graph}()}.
#' @param mode Same as for \code{\link{vis_drake_graph}()}.
#' @param order Same as for \code{\link{vis_drake_graph}()}.
#' @param subset Same as for \code{\link{vis_drake_graph}()}.
#' @param ncol_legend Same as for \code{\link{vis_drake_graph}()}.
#' @param make_imports Same as for \code{\link{vis_drake_graph}()}.
#' @param from_scratch Same as for \code{\link{vis_drake_graph}()}.
#' @param ... Same as for \code{\link{vis_drake_graph}()}.
#' @examples
#' # See ?vis_drake_graph for examples.
plot_graph <- function(
  plan = plan_drake(), targets = drake::possible_targets(plan),
  envir = parent.frame(), verbose = 1,
  hook = default_hook,
  cache = drake::get_cache(verbose = verbose),
  jobs = 1,
  parallelism = drake::default_parallelism(),
  packages = rev(.packages()),
  prework = character(0),
  config = NULL,
  file = character(0),
  selfcontained = FALSE,
  build_times = TRUE,
  digits = 3,
  targets_only = FALSE,
  split_columns = FALSE,
  font_size = 20,
  layout = "layout_with_sugiyama",
  main = NULL,
  direction = "LR",
  hover = TRUE,
  navigationButtons = TRUE, # nolint
  from = NULL,
  mode = c("out", "in", "all"),
  order = NULL,
  subset = NULL,
  ncol_legend = 1,
  make_imports = TRUE,
  from_scratch = FALSE,
  ...
){
  .Deprecated(
    "plot_graph",
    package = "drake",
    msg = paste(
      "drake::plot_graph() is deprecated",
      "due to possible name conflicts.",
      "Use vis_drake_graph() instead."
    )
  )
  vis_drake_graph(
    plan = plan,
    envir = envir,
    verbose = verbose,
    hook = hook,
    cache = cache,
    jobs = jobs,
    parallelism = parallelism,
    packages = packages,
    prework = prework,
    config = config,
    file = file,
    selfcontained = selfcontained,
    build_times = build_times,
    digits = digits,
    targets_only = targets_only,
    split_columns = split_columns,
    font_size = font_size,
    layout = layout,
    main = main,
    direction = direction,
    hover = hover,
    navigationButtons = navigationButtons, # nolint
    from = from,
    mode = mode,
    order = order,
    subset = subset,
    ncol_legend = ncol_legend,
    make_imports = make_imports,
    from_scratch = from_scratch,
    ... = ...
  )
}

#' @title Deprecated function read_config
#' @description Use \code{\link{read_drake_config}()} instead.
#' @details Deprecated on 2017-11-12.
#' @seealso \code{\link{read_drake_config}}
#' @export
#' @keywords internal
#' @return The master internal configuration list of a project.
#' @param path same as \code{\link{read_drake_config}()}
#' @param search same as \code{\link{read_drake_config}()}
#' @param cache same as \code{\link{read_drake_config}()}
#' @param verbose same as \code{\link{read_drake_config}()}
#' @examples
#' # See ?read_drake_config for examples.
read_config <- function(
  path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = TRUE
){
  .Deprecated(
    "read_config",
    package = "drake",
    msg = paste(
      "drake::read_config() is deprecated",
      "due to possible name conflicts.",
      "Use read_drake_config() instead."
    )
  )
  read_drake_config(
    path = path,
    search = search,
    cache = cache,
    verbose = verbose
  )
}

#' @title Deprecated function \code{read_graph}
#' @description Use \code{\link{read_drake_graph}()} instead.
#' @details Deprecated on 2017-11-12.
#' @seealso \code{\link{read_drake_graph}}.
#' @export
#' @keywords internal
#' @return An \code{igraph} object representing the dependency
#' network of the workflow.
#' @param cache Same as for \code{\link{read_drake_graph}()}.
#' @param path Same as for \code{\link{read_drake_graph}()}.
#' @param search Same as for \code{\link{read_drake_graph}()}.
#' @param verbose Same as for \code{\link{read_drake_graph}()}.
#' @param ... Same as for \code{\link{read_drake_graph}()}.
#' @examples
#' # See ?read_drake_graph for the examples.
read_graph <- function(
  path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1, ...
){
  .Deprecated(
    "read_graph",
    package = "drake",
    msg = paste(
      "drake::read_graph() is deprecated",
      "due to possible name conflicts.",
      "Use read_drake_graph() instead."
    )
  )
  read_drake_graph(
    path = path,
    search = search,
    cache = cache,
    verbose = verbose
  )
}

#' @title Deprecated function \code{read_plan}
#' @description Use \code{\link{read_drake_plan}()} instead.
#' @details Deprecated on 2017-11-21.
#' @seealso \code{\link{read_drake_plan}}.
#' @export
#' @keywords internal
#' @return An workflow plan data frame.
#' @param cache Same as for \code{\link{read_drake_plan}()}.
#' @param path Same as for \code{\link{read_drake_plan}()}.
#' @param search Same as for \code{\link{read_drake_plan}()}.
#' @param verbose Same as for \code{\link{read_drake_plan}()}.
#' @param ... Same as for \code{\link{read_drake_plan}()}.
#' @examples
#' # See ?read_drake_plan for the examples.
read_plan <- function(
  path = getwd(), search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1, ...
){
  .Deprecated(
    "read_plan",
    package = "drake",
    msg = paste(
      "drake::read_plan() is deprecated",
      "due to possible name conflicts.",
      "Use read_drake_plan() instead."
    )
  )
  read_drake_plan(
    path = path,
    search = search,
    cache = cache,
    verbose = verbose
  )
}

#' @title Deprecated function \code{render_graph}
#' @description Use \code{\link{render_drake_graph}()} instead.
#' @details Deprecated on 2017-10.
#' @seealso \code{\link{render_drake_graph}}
#' @export
#' @keywords internal
#' @return Same as for \code{\link{render_drake_graph}()}.
#' @param graph_dataframes Same as for \code{\link{render_drake_graph}()}.
#' @param file Same as for \code{\link{render_drake_graph}()}.
#' @param layout Same as for \code{\link{render_drake_graph}()}.
#' @param direction Same as for \code{\link{render_drake_graph}()}.
#' @param hover Same as for \code{\link{render_drake_graph}()}.
#' @param main Same as for \code{\link{render_drake_graph}()}.
#' @param selfcontained Same as for \code{\link{render_drake_graph}()}.
#' @param navigationButtons Same as for \code{\link{render_drake_graph}()}. # nolint
#' @param ncol_legend Same as for \code{\link{render_drake_graph}()}.
#' @param ... Same as for \code{\link{render_drake_graph}()}.
#' @examples
#' # See ?render_drake_graph for examples.
render_graph <- function(
  graph_dataframes,
  file = character(0),
  layout = "layout_with_sugiyama",
  direction = "LR",
  hover = TRUE,
  main = graph_dataframes$default_title,
  selfcontained = FALSE,
  navigationButtons = TRUE, # nolint
  ncol_legend = 1,
  ...
){
  .Deprecated(
    "render_graph",
    package = "drake",
    msg = paste(
      "drake::render_graph() is deprecated",
      "due to possible name conflicts.",
      "Use render_drake_graph() instead."
    )
  )
  render_drake_graph(
    graph_dataframes = graph_dataframes,
    file = file,
    layout = layout,
    direction = direction,
    hover = hover,
    main = main,
    selfcontained = selfcontained,
    navigationButtons = navigationButtons, # nolint
    ncol_legend = ncol_legend,
    ... = ...
  )
}

#' @title Deprecated function \code{session}
#' @description Use \code{\link{drake_session}()} instead
#' @details Deprecated on 2017-11-12.
#' @seealso \code{\link{drake_session}}
#' @export
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
#' @return A data frame of targets and commands.
#' @param ... same as for \code{drake::\link{plan_drake}()}
#' @param list same as for \code{drake::\link{plan_drake}()}
#' @param file_targets same as for \code{drake::\link{plan_drake}()}
#' @param strings_in_dots same as for \code{drake::\link{plan_drake}()}
#' @examples
#' # See ?plan_drake for examples.
workflow <- function(
  ...,
  list = character(0),
  file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")
){
  .Deprecated(
    "workflow",
    package = "drake",
    msg = "workflow() is deprecated. Use plan_drake() instead."
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
    plan$target <- drake::drake_quotes(plan$target, single = TRUE)
  }
  if (strings_in_dots == "filenames"){
    plan$command[from_dots] <- gsub("\"", "'", plan$command[from_dots])
  }
  sanitize_plan(plan)
}

#' @title Function \code{workplan}
#' @description Turns a named collection of command/target pairs into
#' a workflow plan data frame for \code{\link{make}} and
#' \code{\link{check}}.
#' @details Deprecated on 2017-11-29
#' @export
#' @keywords internal
#' @return A data frame of targets and commands.
#' @param ... same as for \code{drake::\link{plan_drake}()}
#' @param list same as for \code{drake::\link{plan_drake}()}
#' @param file_targets same as for \code{drake::\link{plan_drake}()}
#' @param strings_in_dots same as for \code{drake::\link{plan_drake}()}
#' @examples
#' # See ?plan_drake for examples.
workplan <- function(
  ...,
  list = character(0),
  file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")
){
  .Deprecated(
    "plan_drake",
    package = "drake",
    msg = "workplan() is deprecated. Use plan_drake() instead."
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
    plan$target <- drake::drake_quotes(plan$target, single = TRUE)
  }
  if (strings_in_dots == "filenames"){
    plan$command[from_dots] <- gsub("\"", "'", plan$command[from_dots])
  }
  sanitize_plan(plan)
}
