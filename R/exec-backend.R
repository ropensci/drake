process_targets <- function(config) {
  if (is.character(config$parallelism)) {
    run_native_backend(config)
  } else {
    run_external_backend(config)
  }
}

run_native_backend <- function(config) {
  parallelism <- match.arg(
    config$parallelism,
    c("loop", "clustermq", "future")
  )
  config$schedule <- pretrim_schedule(config)
  if (igraph::gorder(config$schedule)) {
    get(
      paste0("backend_", parallelism),
      envir = getNamespace("drake")
    )(config)
  } else {
    console_up_to_date(config = config)
  }
}

run_external_backend <- function(config) {
  warning(
    "`drake` can indeed accept a custom scheduler function for the ",
    "`parallelism` argument of `make()` ",
    "but this is only for the sake of experimentation ",
    "and graceful deprecation. ",
    "Your own custom schedulers may cause surprising errors. ",
    "Use at your own risk.",
    call. = FALSE
  )
  config$parallelism(config = config)
}

pretrim_schedule <- function(config) {
  console_preprocess(text = "trim schedule", config = config)
  outdated <- outdated(config, do_prework = FALSE, make_imports = FALSE)
  igraph::induced_subgraph(graph = config$schedule, vids = outdated)
}
