#' @title Function parallel_stages
#' @description Get information on the parallelizable stages
#' of targets for a workflow.
#' @details Usually, \code{\link{make}()} divides the targets
#' and imports into parallelizable stages strictly according
#' to the columns in \code{\link{vis_drake_graph}()}.
#' However, if some targets are out of date, drake
#' looks ahead in the graph until it finds outdated targets
#' for the current stage. The \code{parallel_stages()} function
#' takes this behavior into account when it reports a data frame
#' of information on how targets and imports will be divided into
#' parallel stages during the next \code{\link{make}()}.
#' @export
#' @seealso \code{\link{make}}, \code{\link{make_with_config}}
#' @return A data frame of information spelling out how
#' targets are divided into parallelizable stages
#' (according to the \code{stage} column).
#' @param config An configuration list output by
#' \code{\link{make}()} or \code{\link{drake_config}()}.
#' @param from_scratch logical, whether to assume
#' that the next \code{\link{make}()} will run from scratch
#' so that all targets are attempted.
#' @examples
#' \dontrun{
#' load_basic_example() # Load the basic example.
#' config <- drake_config(my_plan) # Get a configuration list.
#' # Parallel stages for the next make().
#' parallel_stages(config = config)
#' # Check the graph to see that the information agrees.
#' vis_drake_graph(config = config)
#' # Build the project.
#' config <- make_with_config(config) # or make(my_plan)
#' # Nothing to build in the next make().
#' parallel_stages(config = config)
#' # Change a dependency and notice how the stages change.
#' reg2 = function(d){
#'   d$x3 = d$x^3
#'   lm(y ~ x3, data = d)
#' }
#' parallel_stages(config = config)
#' }
parallel_stages <- function(config, from_scratch = FALSE){
  do_prework(config = config, verbose_packages = TRUE)
  config$store_meta <- FALSE
  if (from_scratch){
    config$trigger <- "always"
  }
  config$stages_cache <- storr::storr_environment()
  config$stages_cache$clear()
  config$execution_graph <- imports_graph(config = config)
  run_parallel(config = config, worker = worker_parallel_stages)
  targets_graph <- targets_graph(config = config)
  config$execution_graph <- targets_graph
  run_parallel(config = config, worker = worker_parallel_stages)
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
  run_parallel(config = config, worker = worker_parallel_stages)
  out <- read_parallel_stages(config = config)
  config$stages_cache$clear()
  out[order(out$stage, decreasing = FALSE), ]
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
