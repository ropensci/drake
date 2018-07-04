initialize_protocol <- function(
  plan, targets, envir, verbose, jobs, console_log_file
){
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
    pattern = "connect",
    type = "import",
    config = config
  )
  import_deps <- lightly_parallelize(
    X = names(imports),
    FUN = function(name){
      out <- import_dependencies(imports[[name]])
      out$name <- name
      out$imported <- TRUE
      out
    },
    jobs = jobs
  )
  names(import_deps) <- names(imports)
  console_many_targets(
    targets = plan$target,
    pattern = "connect",
    type = "target",
    config = config
  )
  command_deps <- lightly_parallelize(
    X = seq_len(nrow(plan)),
    FUN = function(i){
      command <- plan$command[i]
      out <- command_dependencies(command)
      out$name <- plan$target[i]
      out$imported <- FALSE
      out$command <- command
      out
    },
    jobs = jobs
  )
  names(command_deps) <- plan$target
  list2env(c(import_deps, command_deps), parent = emptyenv(), hash = TRUE)
}
