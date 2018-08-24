#' @title Create the `igraph` dependency network of your project.
#' @description This function returns an igraph object representing how
#' the targets in your workflow plan data frame
#' depend on each other.
#' (`help(package = "igraph")`). To plot this graph, call
#' to [plot.igraph()] on your graph. See the online manual
#' for enhanced graph visualization functionality.
#' @export
#' @return An igraph object representing
#'   the workflow plan dependency network.
#' @inheritParams drake_config
#' @param sanitize_plan logical, deprecated. If you must,
#'   call `drake:::sanitize_plan()` to sanitize the plan
#'   and/or `drake:::sanitize_targets()` to sanitize the targets
#'   (or just get `plan` and `targets` and `graph` from
#'   [drake_config()]).
#' @param trigger optional, a global trigger for building targets
#'   (see [trigger()]).
#' @param cache an optional `storr` cache for memoization
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # Make the igraph network connecting all the targets and imports.
#' g <- build_drake_graph(my_plan)
#' class(g) # "igraph"
#' })
#' }
build_drake_graph <- function(
  plan = read_drake_plan(),
  targets = plan$target,
  envir = parent.frame(),
  verbose = drake::default_verbose(),
  jobs = 1,
  sanitize_plan = FALSE,
  console_log_file = NULL,
  trigger = drake::trigger(),
  cache = NULL
){
  force(envir)
  if (sanitize_plan){
    warning(
      "The `sanitize_plan` argument to `build_drake_graph()` is deprecated.",
      call. = FALSE
    )
  }
  config <- list(
    plan = plan,
    targets = targets,
    envir = envir,
    verbose = verbose,
    jobs = jobs,
    cache = cache,
    console_log_file = console_log_file,
    trigger = parse_trigger(trigger = trigger, envir = envir)
  )
  imports <- bdg_prepare_imports(config)
  import_deps <- memo_expr(
    bdg_analyze_imports(config, imports),
    config$cache,
    imports
  )
  command_deps <- memo_expr(
    bdg_analyze_commands(config),
    config$cache,
    config$plan[, c("target", "command")]
  )
  trigger_cols <- intersect(colnames(config$plan), c("target", "trigger"))
  trigger_plan <- config$plan[, trigger_cols]
  triggers <- memo_expr(
    bdg_get_triggers(config),
    config$cache,
    trigger_plan,
    config$trigger
  )
  condition_deps <- memo_expr(
    bdg_get_condition_deps(config, triggers),
    config$cache,
    trigger_plan,
    config$trigger,
    triggers
  )
  change_deps <- memo_expr(
    bdg_get_change_deps(config, triggers),
    config$cache,
    trigger_plan,
    config$trigger,
    triggers
  )
  edges <- memo_expr(
    bdg_create_edges(
      config,
      import_deps,
      command_deps,
      condition_deps,
      change_deps
    ),
    config$cache,
    import_deps,
    command_deps,
    condition_deps,
    change_deps
  )
  attributes <- memo_expr(
    bdg_create_attributes(
      config,
      triggers,
      import_deps,
      command_deps,
      condition_deps,
      change_deps
    ),
    config$cache,
    triggers,
    import_deps,
    command_deps,
    condition_deps,
    change_deps
  )
  memo_expr(
    bdg_create_graph(config, edges, attributes),
    config$cache,
    config$targets,
    edges,
    attributes
  )
}

bdg_prepare_imports <- function(config){
  console_preprocess(text = "analyze environment", config = config)
  imports <- as.list(config$envir)
  unload_conflicts(
    imports = names(imports),
    targets = config$plan$target,
    envir = config$envir,
    verbose = config$verbose
  )
  import_names <- setdiff(names(imports), config$targets)
  imports[import_names]
}

bdg_analyze_imports <- function(config, imports){
  console_many_targets(
    targets = names(imports),
    pattern = "analyze",
    type = "import",
    config = config
  )
  lightly_parallelize(
    X = seq_along(imports),
    FUN = function(i){
      import_dependencies(expr = imports[[i]], exclude = names(imports)[[i]])
    },
    jobs = config$jobs
  ) %>%
    setNames(names(imports))
}

bdg_analyze_commands <- function(config){
  console_many_targets(
    targets = config$plan$target,
    pattern = "analyze",
    type = "target",
    config = config
  )
  lightly_parallelize(
    X = seq_len(nrow(config$plan)),
    FUN = function(i){
      command_dependencies(
        command = config$plan$command[i],
        exclude = config$plan$target[i]
      )
    },
    jobs = config$jobs
  ) %>%
    setNames(config$plan$target)
}

bdg_get_triggers <- function(config){
  if ("trigger" %in% colnames(config$plan)){
    console_preprocess(text = "analyze triggers", config = config)
    triggers <- lightly_parallelize(
      X = seq_len(nrow(config$plan)),
      FUN = function(i){
        if (!safe_is_na(config$plan$trigger[i])){
          parse_trigger(
            trigger = config$plan$trigger[i],
            envir = config$envir
          )
        } else {
          config$trigger
        }
      },
      jobs = config$jobs
    )
  } else {
    triggers <- replicate(
      config$trigger,
      n = nrow(config$plan),
      simplify = FALSE
    )
  }
  setNames(triggers, config$plan$target)
}

bdg_get_condition_deps <- function(config, triggers){
  default_condition_deps <- import_dependencies(config$trigger$condition)
  if ("trigger" %in% colnames(config$plan)){
    console_preprocess(text = "analyze condition triggers", config = config)
    condition_deps <- lightly_parallelize(
      X = seq_len(nrow(config$plan)),
      FUN = function(i){
        if (!safe_is_na(config$plan$trigger[i])){
          import_dependencies(
            expr = triggers[[i]]$condition,
            exclude = config$plan$target[i]
          )
        } else {
          default_condition_deps
        }
      },
      jobs = config$jobs
    )
  } else {
    condition_deps <- replicate(
      default_condition_deps,
      n = nrow(config$plan),
      simplify = FALSE
    )
  }
  setNames(condition_deps, config$plan$target)
}

bdg_get_change_deps <- function(config, triggers){
  default_change_deps <- import_dependencies(config$trigger$change)
  if ("trigger" %in% colnames(config$plan)){
    console_preprocess(text = "analyze change triggers", config = config)
    change_deps <- lightly_parallelize(
      X = seq_len(nrow(config$plan)),
      FUN = function(i){
        if (!safe_is_na(config$plan$trigger[i])){
          import_dependencies(
            expr = triggers[[i]]$change,
            exclude = config$plan$target[i]
          )
        } else {
          default_change_deps
        }
      },
      jobs = config$jobs
    )
  } else {
    change_deps <- replicate(
      default_change_deps,
      n = nrow(config$plan),
      simplify = FALSE
    )
  }
  setNames(change_deps, config$plan$target)
}

bdg_create_edges <- function(
  config,
  import_deps,
  command_deps,
  condition_deps,
  change_deps
){
  console_preprocess(text = "construct graph edges", config = config)
  import_edges <- lightly_parallelize(
    X = seq_along(import_deps),
    FUN = function(i){
      code_deps_to_edges(
        target = names(import_deps)[[i]],
        deps = import_deps[[i]]
      )
    },
    jobs = config$jobs
  ) %>%
    do.call(what = dplyr::bind_rows)
  target_edges <- lightly_parallelize(
    X = seq_along(command_deps),
    FUN = function(i){
      deps <- merge_lists(command_deps[[i]], condition_deps[[i]]) %>%
        merge_lists(change_deps[[i]])
      code_deps_to_edges(target = names(command_deps)[i], deps = deps)
    },
    jobs = config$jobs
  ) %>%
    do.call(what = dplyr::bind_rows)
  if (nrow(target_edges) > 0){
    target_edges <- connect_output_files(
      target_edges,
      command_deps,
      config$jobs
    )
  }
  if (nrow(import_edges) > 0){
    import_edges$file <- FALSE # no input/output file connections here
  }
  dplyr::bind_rows(import_edges, target_edges)
}

bdg_create_attributes <- function(
  config,
  triggers,
  import_deps,
  command_deps,
  condition_deps,
  change_deps
){
  console_preprocess(text = "construct vertex attributes", config = config)
  import_deps_attr <- lightly_parallelize(
    X = import_deps,
    FUN = list2env,
    jobs = config$jobs,
    parent = emptyenv(),
    hash = TRUE
  ) %>%
    setNames(names(import_deps))
  target_deps_attr <- lightly_parallelize(
    X = seq_along(command_deps),
    FUN = zip_deps,
    jobs = config$jobs,
    command_deps = command_deps,
    condition_deps = condition_deps,
    change_deps = change_deps
  ) %>%
    setNames(names(command_deps))
  deps_attr <- c(import_deps_attr, target_deps_attr)
  trigger_attr <- lightly_parallelize(
    X = triggers,
    FUN = list2env,
    jobs = config$jobs,
    parent = emptyenv(),
    hash = TRUE
  ) %>%
    setNames(names(triggers))
  list(deps_attr = deps_attr, trigger_attr = trigger_attr)
}

bdg_create_graph <- function(config, edges, attributes){
  console_preprocess(text = "construct graph", config = config)
  igraph::graph_from_data_frame(edges) %>%
    igraph::set_vertex_attr(
      name = "deps",
      index = names(attributes$deps_attr),
      value = attributes$deps_attr
    ) %>%
    igraph::set_vertex_attr(
      name = "trigger",
      index = names(attributes$trigger_attr),
      value = attributes$trigger_attr
    ) %>%
    prune_drake_graph(to = config$targets, jobs = config$jobs) %>%
    igraph::simplify(
      remove.loops = TRUE,
      remove.multiple = TRUE,
      edge.attr.comb = "min"
    )
}

code_deps_to_edges <- function(target, deps){
  inputs <- clean_dependency_list(deps[setdiff(names(deps), "file_out")])
  edges <- NULL
  if (length(inputs)){
    data.frame(from = inputs, to = target, stringsAsFactors = FALSE)
  } else {
    # Loops will be removed.
    data.frame(from = target, to = target, stringsAsFactors = FALSE)
  }
}

connect_output_files <- function(target_edges, command_deps, jobs){
  output_files <- lightly_parallelize(
    X = seq_along(command_deps),
    FUN = function(i){
      unlist(command_deps[[i]]$file_out)
    },
    jobs = jobs
  ) %>%
    setNames(nm = names(command_deps)) %>%
    select_nonempty
  if (!length(output_files)){
    target_edges$file <- FALSE
    return(target_edges)
  }
  output_files <- utils::stack(output_files)
  output_files$ind <- as.character(output_files$ind)
  index <- match(target_edges$from, table = output_files$values)
  target_edges$from[is.finite(index)] <- output_files$ind[na.omit(index)]
  target_edges$file <- !is.na(index) # mark input/output file connections
  target_edges[!duplicated(target_edges), ]
}

unload_conflicts <- function(imports, targets, envir, verbose){
  common <- intersect(imports, targets)
  if (verbose & length(common)){
    message(
      "Unloading targets from environment:\n",
      multiline_message(common), sep = ""
    )
  }
  remove(list = common, envir = envir)
}

zip_deps <- function(index, command_deps, condition_deps, change_deps){
  out <- command_deps[[index]]
  out$condition <- unlist(condition_deps[[index]]) %>%
    Filter(f = is_not_file) %>%
    clean_dependency_list
  out$change <- unlist(change_deps[[index]]) %>%
    Filter(f = is_not_file) %>%
    clean_dependency_list
  select_nonempty(out) %>%
    list2env(parent = emptyenv(), hash = TRUE)
}
