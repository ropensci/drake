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
  trigger = drake::trigger()
){
  force(envir)
  if (sanitize_plan){
    warning(
      "The `sanitize_plan` argument to `build_drake_graph()` is deprecated.",
      call. = FALSE
    )
  }
  list(
    plan = plan,
    targets = targets,
    envir = envir,
    verbose = verbose,
    jobs = jobs,
    console_log_file = console_log_file,
    trigger = trigger
  ) %>%
    bdg_prepare_data %>%
    bdg_analyze_code %>%
    bdg_analyze_triggers %>%
    bdg_create_edges %>%
    bdg_create_attributes %>%
    bdg_create_graph
}

bdg_prepare_data <- function(args){
  within(args, {
    imports <- as.list(envir)
    unload_conflicts(
      imports = names(imports),
      targets = plan$target,
      envir = envir,
      verbose = verbose
    )
    import_names <- setdiff(names(imports), targets)
    imports <- imports[import_names]
    args
  })
}

bdg_analyze_code <- function(args){
  within(args, {
    console_many_targets(
      targets = import_names,
      pattern = "analyze",
      type = "import",
      config = args
    )
    import_deps <- lightly_parallelize(
      X = imports,
      FUN = import_dependencies,
      jobs = jobs
    ) %>%
      setNames(import_names)
    console_many_targets(
      targets = plan$target,
      pattern = "analyze",
      type = "target",
      config = args
    )
    command_deps <- lightly_parallelize(
      X = plan$command,
      FUN = command_dependencies,
      job = jobs
    ) %>%
      setNames(plan$target)
    args
  })
}

bdg_analyze_triggers <- function(args){
  within(args, {
    default_trigger <- parse_trigger(trigger = trigger, envir = envir)
    default_condition_deps <- code_dependencies(default_trigger$condition)
    default_change_deps <- code_dependencies(default_trigger$change)
    if ("trigger" %in% colnames(plan)){
      triggers <- lightly_parallelize(
        X = seq_len(nrow(plan)),
        FUN = function(i){
          if (!safe_is_na(plan$trigger[i])){
            parse_trigger(trigger = plan$trigger[i], envir = envir)
          } else {
            default_trigger
          }
        },
        jobs = jobs
      )
      condition_deps <- lightly_parallelize(
        X = seq_len(nrow(plan)),
        FUN = function(i){
          if (!safe_is_na(plan$trigger[i])){
            code_dependencies(triggers[[i]]$condition)
          } else {
            default_condition_deps
          }
        },
        jobs = jobs
      )
      change_deps <- lightly_parallelize(
        X = seq_len(nrow(plan)),
        FUN = function(i){
          if (!safe_is_na(plan$trigger[i])){
            code_dependencies(triggers[[i]]$change)
          } else {
            default_change_deps
          }
        },
        jobs = jobs
      )
    } else {
      triggers <- replicate(default_trigger, n = nrow(plan), simplify = FALSE)
      condition_deps <- replicate(
        default_condition_deps,
        n = nrow(plan),
        simplify = FALSE
      )
      change_deps <- replicate(
        default_change_deps,
        n = nrow(plan),
        simplify = FALSE
      )
    }
    triggers <- setNames(triggers, plan$target)
    condition_deps <- setNames(condition_deps, plan$target)
    change_deps <- setNames(change_deps, plan$target)
    args
  })
}

bdg_create_edges <- function(args){
  within(args, {
    import_edges <- lightly_parallelize(
      X = seq_along(imports),
      FUN = function(i){
        code_deps_to_edges(target = import_names[[i]], deps = import_deps[[i]])
      },
      jobs = jobs
    ) %>%
      do.call(what = dplyr::bind_rows)
    target_edges <- lightly_parallelize(
      X = seq_len(nrow(plan)),
      FUN = function(i){
        deps <- merge_lists(command_deps[[i]], condition_deps[[i]]) %>%
          merge_lists(change_deps[[i]])
        code_deps_to_edges(target = plan$target[i], deps = deps)
      },
      jobs = jobs
    ) %>%
      do.call(what = dplyr::bind_rows)
    target_edges <- connect_output_files(target_edges, command_deps, jobs)
    edges <- dplyr::bind_rows(import_edges, target_edges)
    args
  })
}

bdg_create_attributes <- function(args){
  within(args, {
    import_deps_attr <- lightly_parallelize(
      X = import_deps,
      FUN = list2env,
      jobs = jobs,
      parent = emptyenv(),
      hash = TRUE
    ) %>%
      setNames(names(import_deps))
    target_deps_attr <- lightly_parallelize(
      X = seq_len(nrow(plan)),
      FUN = zip_deps,
      jobs = jobs,
      command_deps = command_deps,
      condition_deps = condition_deps,
      change_deps = change_deps
    ) %>%
      setNames(plan$target)
    deps_attr <- c(import_deps_attr, target_deps_attr)
    trigger_attr <- lightly_parallelize(
      X = triggers,
      FUN = list2env,
      jobs = jobs,
      parent = emptyenv(),
      hash = TRUE
    )
    args
  })
}

bdg_create_graph <- function(args){
  with(args, {
    igraph::graph_from_data_frame(edges) %>%
      igraph::set_vertex_attr(
        name = "deps",
        index = names(deps_attr),
        value = deps_attr
      ) %>%
      igraph::set_vertex_attr(
        name = "trigger",
        index = names(trigger_attr),
        value = trigger_attr
      ) %>%
      prune_drake_graph(to = targets, jobs = jobs) %>%
      igraph::simplify(remove.multiple = TRUE, remove.loops = TRUE)
  })
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
    return(target_edges)
  }
  output_files <- utils::stack(output_files)
  output_files$ind <- as.character(output_files$ind)
  index <- match(target_edges$from, table = output_files$values)
  target_edges$from[is.finite(index)] <- output_files$ind[na.omit(index)]
  target_edges
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
