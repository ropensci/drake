# Requires either a formal scheduler or an ssh
# connection to test.
run_clustermq_persistent <- function(config){
  if (!requireNamespace("clustermq")){
    drake_error(
      "drake::make(parallelism = \"clustermq_persistent\") requires ",
      "the clustermq package: https://github.com/mschubert/clustermq.",
      config = config
    )
  }
  prepare_distributed(config = config)
  mc_init_worker_cache(config)
  console_persistent_workers(config)
  path <- normalizePath(config$cache_path, winslash = "/")
  rscript <- grep(
    "Rscript",
    dir(R.home("bin"), full.names = TRUE),
    value = TRUE
  )
  tmp <- system2(
    rscript,
    shQuote(c("-e", paste0("drake::remote_master('", path, "')"))),
    wait = FALSE
  )
  clustermq::Q(
    worker = mc_worker_id(seq_len(config$jobs)),
    cache_path = config$cache_path,
    fun = function(worker, cache_path){
      drake::remote_worker(worker = worker, cache_path = cache_path)
    },
    n_jobs = config$jobs
  )
  finish_distributed(config = config)
}
