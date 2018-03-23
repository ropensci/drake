#' @title Show how [make()] will build your targets
#'   in successive parallelizable stages.
#' @description The stages determine the order in which
#' [make()] builds the targets.
#' @details Usually, [make()] divides the targets
#' and imports into parallelizable stages strictly according
#' to the columns in [vis_drake_graph()].
#' However, if some targets are out of date, drake
#' looks ahead in the graph until it finds outdated targets
#' for the current stage. The `parallel_stages()` function
#' takes this behavior into account when it reports a data frame
#' of information on how targets and imports will be divided into
#' parallel stages during the next [make()].
#' @export
#' @seealso [next_stage()],
#'   [make()], [make_with_config()]
#' @return A data frame of information spelling out how
#'   targets are divided into parallelizable stages
#'   (according to the `stage` column).
#' @param config An configuration list output by
#'   [make()] or [drake_config()].
#' @param from_scratch logical, whether to assume
#'   that the next [make()] will run from scratch
#'   so that all targets are attempted.
#' @examples
#'   \dontrun{
#'   test_with_dir("Quarantine side effects.", {
#'   load_basic_example() # Get the code with drake_example("basic").
#'   config <- drake_config(my_plan) # Get a configuration list.
#'   # Parallel stages for the next make().
#'   parallel_stages(config = config)
#'   # Check the graph to see that the information agrees.
#'   vis_drake_graph(config = config)
#'   # Build the project.
#'   config <- make_with_config(config) # or make(my_plan)
#'   # Nothing to build in the next make().
#'   parallel_stages(config = config)
#'   # Change a dependency and notice how the stages change.
#'   reg2 = function(d){
#'     d$x3 = d$x^3
#'     lm(y ~ x3, data = d)
#'   }
#'   parallel_stages(config = config)
#'   })
#'   }
parallel_stages <- function(
  config = drake::read_drake_config(),
  from_scratch = FALSE
){
  do_prework(config = config, verbose_packages = config$verbose)
  if (from_scratch){
    config$trigger <- "always"
    config$plan$trigger <- "always"
  }
  config$stages_cache <- storr::storr_environment()
  config$stages_cache$clear()
  config$execution_graph <- imports_graph(graph = config$graph)
  run_staged_parallelism(config = config, worker = worker_parallel_stages)
  targets_graph <- targets_graph(graph = config$graph)
  config$execution_graph <- targets_graph
  run_staged_parallelism(config = config, worker = worker_parallel_stages)
  out <- read_parallel_stages(config = config)
  if (!length(out)){
    return(data.frame(
      item = character(0),
      imported = logical(0),
      file = logical(0),
      stage = numeric(0)
    ))
  }
  also_outdated <- downstream_nodes(
    from = out$item[!out$imported],
    graph = config$graph,
    jobs = config$jobs
  ) %>%
    setdiff(y = out$item)
  delete_these <- setdiff(V(targets_graph)$name, also_outdated)
  config$execution_graph <- delete_vertices(
    graph = targets_graph,
    v = delete_these
  )
  config$trigger <- "always"
  run_staged_parallelism(config = config, worker = worker_parallel_stages)
  out <- read_parallel_stages(config = config)
  config$stages_cache$clear()
  out[order(out$stage, decreasing = FALSE), ]
}

run_staged_parallelism <- function(config, worker) {
  config <- exclude_imports_if(config = config)
  while (length(V(config$execution_graph))){
    config <- parallel_stage(worker = worker, config = config)
  }
  invisible()
}

worker_parallel_stages <- function(targets, meta_list, config){
  imports <- setdiff(targets, config$plan$target)
  if (length(imports)){
    worker_mclapply(
      targets = imports,
      meta_list = meta_list,
      config = config
    )
  }
  if (!config$stages_cache$exists(key = "stage")){
    config$stages_cache$set(key = "stage", value = 1)
  }
  stage <- config$stages_cache$get(key = "stage")
  out <- data.frame(
    item = targets,
    imported = !targets %in% config$plan$target,
    file = is_file(targets),
    stage = stage,
    stringsAsFactors = FALSE
  )
  config$stages_cache$set(key = "stage", value = stage + 1)
  config$stages_cache$set(
    key = paste0("stage", stage),
    value = out
  )
  invisible()
}

read_parallel_stages <- function(config){
  keys <- config$stages_cache$list() %>%
    setdiff(y = "stage")
  out <- lightly_parallelize(
    X = keys,
    FUN = function(key){
      config$stages_cache$get(key = key)
    },
    jobs = config$jobs
  ) %>%
    do.call(what = "rbind")
}

#' @title List the targets that will be built in the
#'   first parallelizable stage of the next call to [make()].
#' @description Similar to the first stage in the output of
#' [parallel_stages()].
#' @seealso [parallel_stages()],
#'   [make()], [drake_config()]
#' @export
#' @return A character vector of the targets to be made
#'   in the first parallel stage of the next call to [make()].
#' @param config A master configuration list produced by
#'   [drake_config()] or [make()]
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' config <- load_basic_example() # Get the code with drake_example("basic").
#' next_stage(config = config)    # "small" and "large"
#' })
#' }
next_stage <- function(config = drake::read_drake_config()){
  config$stages_cache <- storr::storr_environment()
  config$stages_cache$clear()
  config$execution_graph <- targets_graph(graph = config$graph)
  parallel_stage(worker = worker_next_stage, config = config)
  tryCatch(
    config$stages_cache$get(key = "next_stage"),
    error = error_character0
  )
}

drake_build_worker <- function(target, meta_list, config){
  build_and_store(
    target = target,
    meta = meta_list[[target]],
    config = config
  )
}

worker_next_stage <- function(targets, meta_list, config){
  config$stages_cache$set(
    key = "next_stage",
    value = targets
  )
}

parallel_stage <- function(worker, config) {
  build_these <- character(0)
  meta_list <- list()
  old_leaves <- NULL
  while (TRUE){
    new_leaves <- leaf_nodes(graph = config$execution_graph) %>%
      setdiff(y = build_these) %>%
      sort
    # Probably will not encounter this, but it prevents
    # an infinite loop:
    if (identical(old_leaves, new_leaves)){ # nocov # nolint
      break                                 # nocov
    }                                       # nocov
    meta_list <- c(
      meta_list,
      meta_list(
        targets = new_leaves,
        config = config
      )
    )
    do_build <- lightly_parallelize(
      X = new_leaves,
      FUN = should_build,
      jobs = config$jobs,
      meta_list = meta_list,
      config = config
    ) %>%
      unlist
    build_these <- c(build_these, new_leaves[do_build])
    if (!all(do_build)){
      trim_these <- new_leaves[!do_build]
      config$execution_graph <- delete_vertices(
        graph = config$execution_graph,
        v = trim_these
      )
    } else {
      break
    }
    old_leaves <- new_leaves
  }
  if (length(intersect(build_these, config$plan$target))){
    set_attempt_flag(config = config)
  }
  if (length(build_these)){
    worker(targets = build_these, meta_list = meta_list,
           config = config)
  }
  config$execution_graph <-
    delete_vertices(config$execution_graph, v = build_these)
  invisible(config)
}
