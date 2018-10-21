run_blitz <- function(config){
  config$schedule <- targets_graph(config = config)
  if (config$jobs_targets > 1L){
    blitz_parallel(config)
  } else{
    blitz_loop(config)
  }
}

blitz_build <- function(target, config){
  console_target(target = target, config = config)
  config$envir[[target]] <- eval(
    expr = preprocess_command(target, config),
    envir = config$envir
  )
}

blitz_loop <- function(config){
  targets <- igraph::topo_sort(config$schedule)$name
  for (target in targets){
    blitz_build(target = target, config = config)
  }
  invisible()
}

blitz_parallel <- function(config){
  
}
