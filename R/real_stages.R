real_stages <- function(config){
  config$cache$clear(namespace = "real_stages")
  config$execution_graph <- imports_graph(config = config)
  resolve_real_stages(config = config)
  config$execution_graph <- targets_graph(config = config)
  resolve_real_stages(config = config)
  keys <- config$cache$list(namespace = "real_stages") %>%
    setdiff(y = "stage")
  out <- lightly_parallelize(
    X = keys,
    FUN = function(key){
      config$cache$get(key = key, namespace = "real_stages")
    },
    jobs = config$jobs
  ) %>%
    do.call(what = "rbind")
  config$cache$clear(namespace = "real_stages")
  out[order(out$stage, decreasing = FALSE), ]
}

resolve_real_stages <- function(config){
  do_prework(config = config, verbose_packages = TRUE)
  config$store_meta <- FALSE
  run_parallel(config = config, worker = worker_real_stages)
}

worker_real_stages <- function(targets, meta_list, config){
  if (!config$cache$exists(key = "stage", namespace = "real_stages")){
    config$cache$set(key = "stage", value = 1, namespace = "real_stages")
  }
  stage <- config$cache$get(key = "stage", namespace = "real_stages")
  out <- data.frame(
    item = targets,
    imported = !targets %in% config$plan$target,
    file = is_file(targets),
    stage = stage
  )
  config$cache$set(key = "stage", value = stage + 1, namespace = "real_stages")
  config$cache$set(
    key = paste0("stage", stage),
    value = out,
    namespace = "real_stages"
  )
  invisible()
}
