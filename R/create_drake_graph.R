create_drake_graph <- function(
  plan = read_drake_plan(),
  targets = plan$target,
  envir = parent.frame(),
  verbose = drake::default_verbose(),
  jobs = 1,
  console_log_file = NULL,
  trigger = drake::trigger(),
  cache = NULL
){
  force(envir)
  config <- list(
    plan = plan,
    targets = targets,
    envir = envir,
    verbose = verbose,
    jobs = jobs,
    cache = cache,
    console_log_file = console_log_file,
    trigger = parse_trigger(trigger = trigger, envir = envir),
    globals = sort(c(plan$target, ls(envir = envir, all.names = TRUE)))
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
    config$plan[, c("target", "command")],
    import_deps
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
    triggers,
    import_deps,
    command_deps
  )
  change_deps <- memo_expr(
    bdg_get_change_deps(config, triggers),
    config$cache,
    trigger_plan,
    config$trigger,
    triggers,
    import_deps,
    command_deps
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
  out <- lightly_parallelize(
    X = seq_along(imports),
    FUN = function(i){
      import_dependencies(
        expr = imports[[i]],
        exclude = names(imports)[[i]],
        globals = config$globals
      )
    },
    jobs = config$jobs
  )
  names(out) <- names(imports)
  out
}

bdg_analyze_commands <- function(config){
  console_many_targets(
    targets = config$plan$target,
    pattern = "analyze",
    type = "target",
    config = config
  )
  out <- lightly_parallelize(
    X = seq_len(nrow(config$plan)),
    FUN = function(i){
      command_dependencies(
        command = config$plan$command[i],
        exclude = config$plan$target[i],
        globals = config$globals
      )
    },
    jobs = config$jobs
  )
  names(out) <- config$plan$target
  out
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
  names(triggers) <- config$plan$target
  triggers
}

bdg_get_condition_deps <- function(config, triggers){
  default_condition_deps <- import_dependencies(
    config$trigger$condition,
    globals = config$globals
  )
  if ("trigger" %in% colnames(config$plan)){
    console_preprocess(text = "analyze condition triggers", config = config)
    condition_deps <- lightly_parallelize(
      X = seq_len(nrow(config$plan)),
      FUN = function(i){
        if (!safe_is_na(config$plan$trigger[i])){
          import_dependencies(
            expr = triggers[[i]]$condition,
            exclude = config$plan$target[i],
            globals = config$globals
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
  names(condition_deps) <- config$plan$target
  condition_deps
}

bdg_get_change_deps <- function(config, triggers){
  default_change_deps <- import_dependencies(
    config$trigger$change,
    globals = config$globals
  )
  if ("trigger" %in% colnames(config$plan)){
    console_preprocess(text = "analyze change triggers", config = config)
    change_deps <- lightly_parallelize(
      X = seq_len(nrow(config$plan)),
      FUN = function(i){
        if (!safe_is_na(config$plan$trigger[i])){
          import_dependencies(
            expr = triggers[[i]]$change,
            exclude = config$plan$target[i],
            globals = config$globals
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
  names(change_deps) <- config$plan$target
  change_deps
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
  )
  import_edges <- do.call(import_edges, what = dplyr::bind_rows)
  target_edges <- lightly_parallelize(
    X = seq_along(command_deps),
    FUN = function(i){
      deps <- merge_lists(command_deps[[i]], condition_deps[[i]])
      deps <- merge_lists(deps, change_deps[[i]])
      code_deps_to_edges(target = names(command_deps)[i], deps = deps)
    },
    jobs = config$jobs
  )
  target_edges <- do.call(target_edges, what = dplyr::bind_rows)
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
  )
  names(import_deps_attr) <- names(import_deps)
  target_deps_attr <- lightly_parallelize(
    X = seq_along(command_deps),
    FUN = zip_deps,
    jobs = config$jobs,
    command_deps = command_deps,
    condition_deps = condition_deps,
    change_deps = change_deps
  )
  names(target_deps_attr) <- names(command_deps)
  deps_attr <- c(import_deps_attr, target_deps_attr)
  trigger_attr <- lightly_parallelize(
    X = triggers,
    FUN = list2env,
    jobs = config$jobs,
    parent = emptyenv(),
    hash = TRUE
  )
  names(trigger_attr) <- names(triggers)
  list(deps_attr = deps_attr, trigger_attr = trigger_attr)
}

bdg_create_graph <- function(config, edges, attributes){
  console_preprocess(text = "construct graph", config = config)
  graph <- igraph::graph_from_data_frame(edges)
  graph <- igraph::set_vertex_attr(
    graph,
    name = "deps",
    index = names(attributes$deps_attr),
    value = attributes$deps_attr
  )
  graph <- igraph::set_vertex_attr(
    graph,
    name = "trigger",
    index = names(attributes$trigger_attr),
    value = attributes$trigger_attr
  )
  graph <- prune_drake_graph(graph, to = config$targets, jobs = config$jobs)
  igraph::simplify(
    graph,
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
  )
  names(output_files) <- names(command_deps)
  output_files <- select_nonempty(output_files)
  if (!length(output_files)){
    target_edges$file <- FALSE
    return(target_edges)
  }
  output_files <- utils::stack(output_files)
  output_files$ind <- as.character(output_files$ind)
  index <- match(target_edges$from, table = output_files$values)
  index_finite <- index[!is.na(index)]
  target_edges$from[is.finite(index)] <- output_files$ind[index_finite]
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
  out$condition <- unlist(condition_deps[[index]])
  out$condition <- clean_dependency_list(out$condition)
  out$change <- unlist(change_deps[[index]])
  out$change <- clean_dependency_list(out$change)
  out <- select_nonempty(out)
  list2env(out, parent = emptyenv(), hash = TRUE)
}
