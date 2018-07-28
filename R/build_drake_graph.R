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
  imports <- as.list(envir)
  unload_conflicts(
    imports = names(imports),
    targets = plan$target,
    envir = envir,
    verbose = verbose
  )
  import_names <- setdiff(names(imports), targets)
  imports <- imports[import_names]
  config <- list(verbose = verbose, console_log_file = console_log_file)
  console_many_targets(
    targets = names(imports),
    pattern = "analyze",
    type = "import",
    config = config
  )

  ####################
  ### DEPENDENCIES ###
  ####################

  # Get the dependencies if all the imports.
  # Each element of import_deps is a list of dependency objects and files
  # categorized by type (objects, file outputs, file inputs, etc).
  import_deps <- lightly_parallelize(
    X = imports,
    FUN = import_dependencies,
    jobs = jobs
  ) %>%
    setNames(import_names)

  # The procedure for targets is similar.
  console_many_targets(
    targets = plan$target,
    pattern = "analyze",
    type = "target",
    config = config
  )

  # We get most of the targets' dependency information
  # from their commands.
  command_deps <- lightly_parallelize(
    X = plan$command,
    FUN = command_dependencies,
    job = jobs
  ) %>%
    setNames(plan$target)

  # But targets also depend on whatever code is in the `condition`
  # and `change` components of the triggers. Below, we parse
  # the `trigger` argument passed through `make()`
  # and get the dependency information of the "condition"
  # and "change" triggers.
  default_trigger <- parse_trigger(trigger = trigger, envir = envir)
  default_condition_deps <- code_dependencies(trigger$condition)
  default_change_deps <- code_dependencies(trigger$change)

  # The user can also give a column of target-specific triggers in the plan.
  # These should override the `trigger` argument to `make()` if supplied.
  # But usually, there will be no custom `trigger` column and we should
  # just use the `trigger` argument to `make()`.
  if ("trigger" %in% colnames(plan)){
    trigger <- lightly_parallelize(
      X = seq_len(nrow(plan)),
      FUN = function(i){
        if (!is.na(plan$trigger[i])){
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
        if (!is.na(plan$trigger[i])){
          code_dependencies(trigger[[i]]$condition)
        } else {
          default_condition_deps
        }
      },
      jobs = jobs
    )
    change_deps <- lightly_parallelize(
      X = seq_len(nrow(plan)),
      FUN = function(i){
        if (!is.na(plan$trigger[i])){
          code_dependencies(trigger[[i]]$change)
        } else {
          default_change_deps
        }
      },
      jobs = jobs
    )
  } else {
    trigger <- replicate(default_trigger, n = nrow(plan), simplify = FALSE)
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
  trigger <- setNames(trigger, plan$target)
  condition_deps <- setNames(condition_deps, plan$target)
  change_deps <- setNames(change_deps, plan$target)

  #############
  ### EDGES ###
  #############
  
  # Convert the dependency information from import_deps
  # into a data frame with columns "from" and "to",
  # where each edge denotes a dependency relationship between
  # two imports. This data frame will be used to construct
  # a large section of our dependency graph, represented by an
  # 'igraph' object. The directed edges in the graph
  # dictate the order in which the imports can be processed.
  import_edges <- lightly_parallelize(
    X = seq_along(imports),
    FUN = function(i){
      code_deps_to_edges(target = import_names[[i]], deps = import_deps[[i]])
    },
    jobs = jobs
  ) %>%
    do.call(what = dplyr::bind_rows)
  
  # The edges for our targets are similar to `import_edges` except that our
  # dependency information comes from commands and triggers.
  # The directed edges in the graph
  # dictate the order in which the targets can be built.
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

  # We're almost ready to construct the graph, but there's one problem:
  # There may be mentions of file_out() files
  # in the `from` column of `target_edges`.
  # We want to convert these file names to the names of the targets
  # that generate them.
  target_edges <- connect_output_files(target_edges, command_deps, jobs)
  edges <- dplyr::bind_rows(import_edges, target_edges)

  ####################
  ### ATTRIBUTES ###
  ####################
  
  # The triggers and dependency relationships will be
  # encoded as igraph attributes. Each attribute must have length 1,
  # so we will convert all our lists to environments.
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
    X = trigger,
    FUN = list2env,
    jobs = jobs,
    parent = emptyenv(),
    hash = TRUE
  )

  # Now, we can make our graph.
  graph <- igraph::graph_from_data_frame(edges) %>%
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
