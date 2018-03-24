#' @title Deprecated function `analyses`
#' @description Use [plan_analyses()] instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @seealso [plan_analyses()]
#' @return The same return value as [plan_analyses()].
#' @param plan Same as for [plan_analyses()].
#' @param datasets Same as for [plan_analyses()].
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

#' @title Deprecated function `as_drake_filename`
#' @description Use [file_store()] instead.
#' @details Deprecated on 2018-03-02.
#' @export
#' @keywords internal
#' @seealso [file_store()]
#' @return The same return value as [file_store()].
#' @param x Same as for [file_store()].
#' @examples
#' # See ?file_store for examples.
as_drake_filename <- function(x){
  .Deprecated(
    "backend",
    package = "drake",
    msg = paste(
      "drake::as_drake_filename() is deprecated.",
      "Use file_store() instead."
    )
  )
  file_store(x)
}

#' @title Deprecated function `as_file`
#' @description Use [file_store()] instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @seealso [file_store()]
#' @return The same return value as [file_store()].
#' @param x Same as for [file_store()].
#' @examples
#' # See ?file_store for examples.
as_file <- function(x){
  .Deprecated(
    "backend",
    package = "drake",
    msg = paste(
      "drake::as_file() is deprecated",
      "due to possible name conflicts.",
      "Use file_store() instead."
    )
  )
  file_store(x)
}

#' @title Deprecated function `backend`
#' @description Use [future::plan()] instead.
#' Avoid `drake::plan()`.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @return The same return value as `future::plan()`.
#' @param ... Arguments to `future::plan()`.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' # Choose future's multicore parallel backend.
#' library(future)
#' future::plan(multicore) # Instead of backend(). Avoid drake::plan().
#' # Run the project, build the targets.
#' # Future knows that you chose the multicore backend.
#' make(my_plan, parallelism = "future_lapply")
#' })
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

#' @title Deprecated function `build_graph`
#' @description Use [build_drake_graph()] instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @seealso [build_drake_graph()]
#' @return The same return value as [build_drake_graph()].
#' @param plan Same as for [build_drake_graph()].
#' @param targets Same as for [build_drake_graph()].
#' @param envir Same as for [build_drake_graph()].
#' @param verbose Same as for [build_drake_graph()].
#' @param jobs Same as for [build_drake_graph()].
#' @examples
#' # See ?as_drake_filename for examples.
build_graph <- function(
  plan = read_drake_plan(),
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

#' @title Deprecated function `check`
#' @description Use [check_plan()] instead.
#' @details Deprecated on 2017-11-12.
#' @seealso [check_plan()]
#' @export
#' @keywords internal
#' @return Same as for [check_plan()].
#' @param plan Same as for [check_plan()].
#' @param targets Same as for [check_plan()].
#' @param envir Same as for [check_plan()].
#' @param cache Same as for [check_plan()].
#' @param verbose Same as for [check_plan()].
#' @examples
#' # See ?check_plan for examples.
check <- function(
  plan = read_drake_plan(),
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

#' @title Deprecated function `config`
#' @description Use [drake_config()] instead.
#' @details Deprecated on 2017-11-12.
#' @seealso [drake_config()]
#' @export
#' @keywords internal
#' @return The master internal configuration list of a project.
#' @seealso [drake_config()]
#' @param plan same as for [make()]
#' @param targets same as for [make()]
#' @param envir same as for [make()]
#' @param verbose same as for [make()]
#' @param hook same as for [make()]
#' @param parallelism same as for [make()]
#' @param jobs same as for [make()]
#' @param packages same as for [make()]
#' @param prework same as for [make()]
#' @param prepend same as for [make()]
#' @param command same as for [make()]
#' @param args same as for [make()]
#' @param recipe_command same as for [make()]
#' @param cache same as for [make()]
#' @param timeout same as for [make()]
#' @param cpu same as for [make()]
#' @param elapsed same as for [make()]
#' @param retries same as for [make()]
#' @param force same as for [make()]
#' @param log_progress same as for [drake_config()]
#' @param graph same as for [drake_config()]
#' @param trigger same as for [make()]
#' @param skip_imports same as for [drake_config()]
#' @examples
#' # See ?drake_config for the examples.
config <- function(
  plan = read_drake_plan(),
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
  log_progress = FALSE,
  graph = NULL,
  trigger = "any",
  skip_imports = FALSE
){
  .Deprecated(
    "evaluate",
    package = "drake",
    msg = paste(
      "drake::config() is deprecated",
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
    log_progress = log_progress,
    graph = graph,
    trigger = trigger,
    skip_imports = skip_imports
  )
}

#' @title Deprecated function `default_system2_args`
#' @description Use [default_Makefile_args()] instead.
#' @details Deprecated on 2017-10.
#' @seealso [default_Makefile_args()]
#' @export
#' @keywords internal
#' @return `args` for \code{\link{system2}(command, args)}
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

# Deprecated on 2018-02-15
doc_of_function_call <- function(expr){
  args <- as.list(expr)[-1]
  if (!length(args)){
    return(character(0))
  }
  if (is.null(names(args))){
    names(args) <- rep("", length(args))
  }
  if (!is.null(args$input)){
    as.character(args$input)
  } else {
    unnamed <- which(!nzchar(names(args)))
    if (!length(unnamed)){
      return(character(0))
    }
    input_index <- min(unnamed)
    as.character(args[[input_index]])
  }
}

#' @title Deprecated function `evaluate`
#' @description Use [evaluate_plan()] instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @seealso [evaluate_plan()]
#' @return Same as for [evaluate_plan()]
#' @param plan Same as for [evaluate_plan()]
#' @param rules Same as for [evaluate_plan()]
#' @param wildcard Same as for [evaluate_plan()]
#' @param values Same as for [evaluate_plan()]
#' @param expand Same as for [evaluate_plan()]
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

#' @title Deprecated function `example_drake`
#' @description Use [drake_example()] instead.
#' @details Deprecated on 2017-11-12.
#' @seealso [drake_example()]
#' @export
#' @keywords internal
#' @return `NULL`
#' @param example Same as for [drake_example()]
#' @param destination Same as for [drake_example()]
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

#' @title Deprecated function `examples_drake`
#' @description Use [drake_examples()] instead.
#' @details Deprecated on 2017-11-12.
#' @seealso [drake_examples()]
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

#' @title Deprecated function `expand`
#' @description Use [expand_plan()] instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @seealso [expand_plan()]
#' @return Same as for [expand_plan()]
#' @param plan Same as for [expand_plan()]
#' @param values Same as for [expand_plan()]
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

#' @title Deprecated function `gather`
#' @description Use [gather_plan()] instead.
#' @details Deprecated on 2017-11-12.
#' @export
#' @keywords internal
#' @seealso [gather_plan()]
#' @return Same as for [gather_plan()]
#' @param plan Same as for [gather_plan()]
#' @param target Same as for [gather_plan()]
#' @param gather Same as for [gather_plan()]
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

# Deprecated on 2018-02-15
find_knitr_doc <- function(expr, result = character(0)){
  if (!length(expr)){
    return(result)
  }
  if (is.character(expr)) {
    if (is_parsable(expr)) {
      expr <- parse(text = expr)
    } else {
      return(result)
    }
  }
  if (is.function(expr)){
    result <- find_knitr_doc(body(expr), result = result)
  } else if (is.call(expr) & length(expr) > 1){
    does_knitting <-
      is_function_call(expr, package = "knitr", what = "knit") ||
      is_function_call(expr, package = "rmarkdown", what = "render")
    if (does_knitting){
      result <- doc_of_function_call(expr)
    } else {
      result <- lapply(as.list(expr), find_knitr_doc,
                       result = result) %>%
        clean_dependency_list
    }
  } else if (is.recursive(expr)){
    result <- lapply(as.list(expr), find_knitr_doc,
                     result = result) %>%
      clean_dependency_list
  }
  setdiff(result, drake_fn_patterns)
}

# Deprecated on 2018-02-15
is_function_call <- function(
  expr,
  package = c("drake", "knitr", "rmarkdown"),
  what = c("loadd", "readd", "knit", "render")
){
  package <- match.arg(package)
  what <- match.arg(what)
  drake::drake_unquote(deparse(expr[[1]])) %in%
    paste0(c("", paste0(package, c("::", ":::"))), what)
}

#' @title Deprecated function `plan`
#' @description Use [drake_plan()] instead.
#' @details Deprecated on 2017-10.
#' @seealso [drake_plan()]
#' @export
#' @keywords internal
#' @return A data frame of targets and commands.
#' @param ... Same as for [drake_plan()].
#' @param list Same as for [drake_plan()].
#' @param file_targets Same as for [drake_plan()].
#' @param strings_in_dots Same as for [drake_plan()].
#' @examples
#' # See ?drake_plan for examples.
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
      "Use drake_plan() instead."
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

#' @title Deprecated function `plan_drake`
#' @description Use [drake_plan()] instead.
#' @details Deprecated on 2017-12-12.
#' @seealso [drake_plan()]
#' @export
#' @keywords internal
#' @return A data frame of targets and commands.
#' @param ... Same as for [drake_plan()].
#' @param list Same as for [drake_plan()].
#' @param file_targets Same as for [drake_plan()].
#' @param strings_in_dots Same as for [drake_plan()].
#' @examples
#' # See ?drake_plan for examples.
plan_drake <- function(
  ...,
  list = character(0),
  file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")
){
  .Deprecated(
    "plan_drake",
    package = "drake",
    msg = paste(
      "plan_drake() is deprecated.",
      "Use drake_plan() instead."
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

#' @title Deprecated function `plot_graph`
#' @description Use [vis_drake_graph()] instead.
#' @details Deprecated on 2017-10.
#' @seealso [vis_drake_graph()]
#' @export
#' @keywords internal
#' @return Same as for [vis_drake_graph()].
#' @param plan Same as for [vis_drake_graph()].
#' @param envir Same as for [vis_drake_graph()].
#' @param hook Same as for [vis_drake_graph()].
#' @param cache Same as for [vis_drake_graph()].
#' @param jobs Same as for [vis_drake_graph()].
#' @param packages Same as for [vis_drake_graph()].
#' @param prework Same as for [vis_drake_graph()].
#' @param verbose Same as for [vis_drake_graph()].
#' @param config Same as for [vis_drake_graph()].
#' @param file Same as for [vis_drake_graph()].
#' @param build_times Same as for [vis_drake_graph()].
#' @param digits Same as for [vis_drake_graph()].
#' @param targets_only Same as for [vis_drake_graph()].
#' @param split_columns Same as for [vis_drake_graph()].
#' @param font_size Same as for [vis_drake_graph()].
#' @param layout Same as for [vis_drake_graph()].
#' @param main Same as for [vis_drake_graph()].
#' @param direction Same as for [vis_drake_graph()].
#' @param hover Same as for [vis_drake_graph()].
#' @param navigationButtons Same as for [vis_drake_graph()]. # nolint
#' @param from Same as for [vis_drake_graph()].
#' @param mode Same as for [vis_drake_graph()].
#' @param order Same as for [vis_drake_graph()].
#' @param subset Same as for [vis_drake_graph()].
#' @param ncol_legend Same as for [vis_drake_graph()].
#' @param make_imports Same as for [vis_drake_graph()].
#' @param from_scratch Same as for [vis_drake_graph()].
#' @param ... Same as for [vis_drake_graph()].
#' @examples
#' # See ?vis_drake_graph for examples.
plot_graph <- function(
  plan = read_drake_plan(), targets = drake::possible_targets(plan),
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

#' @title Deprecated function `read_config`
#' @description Use [read_drake_config()] instead.
#' @details Deprecated on 2017-11-12.
#' @seealso [read_drake_config()]
#' @export
#' @keywords internal
#' @return The master internal configuration list of a project.
#' @param path same as [read_drake_config()]
#' @param search same as [read_drake_config()]
#' @param cache same as [read_drake_config()]
#' @param verbose same as [read_drake_config()]
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

#' @title Deprecated function `read_graph`
#' @description Use [read_drake_graph()] instead.
#' @details Deprecated on 2017-11-12.
#' @seealso [read_drake_graph()].
#' @export
#' @keywords internal
#' @return An `igraph` object representing the dependency
#' network of the workflow.
#' @param cache Same as for [read_drake_graph()].
#' @param path Same as for [read_drake_graph()].
#' @param search Same as for [read_drake_graph()].
#' @param verbose Same as for [read_drake_graph()].
#' @param ... Same as for [read_drake_graph()].
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

#' @title Deprecated: read the metadata of a target or import.
#' @description Use [diagnose()] instead.
#' @details Deprecated on 2018-02-09
#' @seealso [diagnose()] [dependency_profile()], [make()]
#' @keywords internal
#' @export
#' @return The cached master internal configuration list
#'   of the last [make()].
#'
#' @inheritParams cached
#'
#' @param targets character vector, names of the targets
#'   to get metadata. If `NULL`, all metadata is collected.
#'
#' @param verbose whether to print console messages
#'
#' @param jobs number of jobs for light parallelism.
#'   Supports 1 job only on Windows.
#'
#' @examples
#' # See ?diagnose() for examples.
read_drake_meta <- function(
  targets = NULL,
  path = getwd(),
  search = TRUE,
  cache = NULL,
  verbose = 1,
  jobs = 1
){
  .Deprecated(
    "read_drake_meta",
    package = "drake",
    msg = paste(
      "read_drake_meta() is deprecated.",
      "Use diagnose() instead."
    )
  )
  if (is.null(cache)) {
    cache <- get_cache(path = path, search = search, verbose = verbose)
  }
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  if (is.null(targets)){
    targets <- cache$list(namespace = "meta")
  } else {
    targets <- parallel_filter(
      x = targets,
      f = function(target){
        cache$exists(key = target, namespace = "meta")
      },
      jobs = jobs
    )
  }
  out <- lightly_parallelize(
    X = targets,
    FUN = function(target){
      cache$get(key = target, namespace = "meta")
    },
    jobs = jobs
  )
  names(out) <- targets
  if (length(out) == 1){
    out <- out[[1]]
  }
  out
}

#' @title Deprecated function `read_plan`
#' @description Use [read_drake_plan()] instead.
#' @details Deprecated on 2017-11-21.
#' @seealso [read_drake_plan()].
#' @export
#' @keywords internal
#' @return An workflow plan data frame.
#' @param cache Same as for [read_drake_plan()].
#' @param path Same as for [read_drake_plan()].
#' @param search Same as for [read_drake_plan()].
#' @param verbose Same as for [read_drake_plan()].
#' @param ... Same as for [read_drake_plan()].
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

#' @title Deprecated function `render_graph`
#' @description Use [render_drake_graph()] instead.
#' @details Deprecated on 2017-10.
#' @seealso [render_drake_graph()]
#' @export
#' @keywords internal
#' @return Same as for [render_drake_graph()].
#' @param graph_dataframes Same as for [render_drake_graph()].
#' @param file Same as for [render_drake_graph()].
#' @param layout Same as for [render_drake_graph()].
#' @param direction Same as for [render_drake_graph()].
#' @param hover Same as for [render_drake_graph()].
#' @param main Same as for [render_drake_graph()].
#' @param selfcontained Same as for [render_drake_graph()].
#' @param navigationButtons Same as for [render_drake_graph()]. # nolint
#' @param ncol_legend Same as for [render_drake_graph()].
#' @param ... Same as for [render_drake_graph()].
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

#' @title Deprecated function `session`
#' @description Use [drake_session()] instead
#' @details Deprecated on 2017-11-12.
#' @seealso [drake_session()]
#' @export
#' @keywords internal
#' @return Same as for [drake_session()].
#' @param cache Same as for [drake_session()].
#' @param path Same as for [drake_session()].
#' @param search Same as for [drake_session()].
#' @param verbose Same as for [drake_session()].
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

#' @title Deprecated function `summaries`
#' @description Use [summaries()] instead
#' @details Deprecated on 2017-11-12.
#' @seealso [summaries()]
#' @export
#' @keywords internal
#' @return Same as for [plan_summaries()].
#' @param plan Same as for [plan_summaries()].
#' @param analyses Same as for [plan_summaries()].
#' @param datasets Same as for [plan_summaries()].
#' @param gather Same as for [plan_summaries()].
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

#' @title Deprecated function `workflow`
#' @description Turns a named collection of command/target pairs into
#' a workflow plan data frame for [make()] and
#' [check()].
#' @details Deprecated on 2017-10
#' @export
#' @keywords internal
#' @return A data frame of targets and commands.
#' @param ... same as for \code{drake::\link{drake_plan}()}
#' @param list same as for \code{drake::\link{drake_plan}()}
#' @param file_targets same as for \code{drake::\link{drake_plan}()}
#' @param strings_in_dots same as for \code{drake::\link{drake_plan}()}
#' @examples
#' # See ?drake_plan for examples.
workflow <- function(
  ...,
  list = character(0),
  file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")
){
  .Deprecated(
    "workflow",
    package = "drake",
    msg = "workflow() is deprecated. Use drake_plan() instead."
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

#' @title Deprecated function `workplan`
#' @description Turns a named collection of command/target pairs into
#' a workflow plan data frame for [make()] and
#' [check()].
#' @details Deprecated on 2017-11-29
#' @export
#' @keywords internal
#' @return A data frame of targets and commands.
#' @param ... same as for \code{drake::\link{drake_plan}()}
#' @param list same as for \code{drake::\link{drake_plan}()}
#' @param file_targets same as for \code{drake::\link{drake_plan}()}
#' @param strings_in_dots same as for \code{drake::\link{drake_plan}()}
#' @examples
#' # See ?drake_plan for examples.
workplan <- function(
  ...,
  list = character(0),
  file_targets = FALSE,
  strings_in_dots = c("filenames", "literals")
){
  .Deprecated(
    "drake_plan",
    package = "drake",
    msg = "workplan() is deprecated. Use drake_plan() instead."
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
