create_drake_ordinances <- function(
  plan = read_drake_plan(),
  targets = plan$target,
  envir = parent.frame(),
  verbose = drake::default_verbose(),
  jobs = 1,
  console_log_file = NULL,
  trigger = drake::trigger(),
  cache = NULL
) {
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
  imports <- cdn_prepare_imports(config)
  import_ordinances <- memo_expr(
    cdn_analyze_imports(config, imports),
    config$cache,
    imports
  )
  command_ordinances <- memo_expr(
    cdn_analyze_commands(config),
    config$cache,
    config$plan,
    config$trigger,
    config$globals,
    import_ordinances
  )
  c(import_ordinances, command_ordinances)
}

cdn_prepare_imports <- function(config) {
  console_preprocess(text = "analyze environment", config = config)
  imports <- as.list(config$envir)
  cdn_unload_conflicts(
    imports = names(imports),
    targets = config$plan$target,
    envir = config$envir,
    verbose = config$verbose
  )
  import_names <- setdiff(names(imports), config$targets)
  imports[import_names]
}

cdn_unload_conflicts <- function(imports, targets, envir, verbose) {
  common <- intersect(imports, targets)
  if (verbose & length(common)) {
    message(
      "Unloading targets from environment:\n",
      multiline_message(common), sep = ""
    )
  }
  remove(list = common, envir = envir)
}

cdn_analyze_imports <- function(config, imports) {
  names <-  names(imports)
  console_many_targets(
    targets = names,
    pattern = "analyze",
    type = "import",
    config = config
  )
  out <- lightly_parallelize(
    X = seq_along(imports),
    FUN = function(i) {
      list(
        target = names[i],
        deps_build = import_dependencies(
          expr = imports[[i]],
          exclude = names(imports)[[i]],
          globals = config$globals
        ),
        imported = TRUE
      )
    },
    jobs = config$jobs
  )
  names(out) <- names(imports)
  out
}

cdn_analyze_commands <- function(config) {
  console_many_targets(
    targets = config$plan$target,
    pattern = "analyze",
    type = "target",
    config = config
  )
  config$plan$imported <- FALSE
  if ("trigger" %in% colnames(config$plan)) {
    config$plan$trigger <- lapply(
      config$plan$trigger,
      parse_trigger,
      envir = config$envir
    )
  }
  ordinances <- purrr::pmap(.l = config$plan, .f = list)
  names(ordinances) <- config$plan$target
  config$default_condition_deps <- import_dependencies(
    config$trigger$condition,
    globals = config$globals
  )
  config$default_change_deps <- import_dependencies(
    config$trigger$change,
    globals = config$globals
  )
  out <- lightly_parallelize(
    X = ordinances,
    FUN = cdn_prepare_ordinance,
    jobs = config$jobs,
    config = config
  )
  names(out) <- config$plan$target
  out
}

cdn_prepare_ordinance <- function(ordinance, config){
  ordinance$deps_build <- command_dependencies(
    command = ordinance$command,
    exclude = ordinance$target,
    globals = config$globals
  )
  ordinance$command_standardized <- standardize_command(ordinance$command)
  ordinance$command_build <- preprocess_command(
    ordinance$command,
    config = config
  )
  if (is.null(ordinance$trigger) || is.na(ordinance$trigger)){
    ordinance$trigger <- config$trigger
    ordinance$deps_condition <- config$default_condition_deps
    ordinance$deps_change <- config$default_change_deps
  } else {
    ordinance$deps_condition <- import_dependencies(
      ordinance$trigger$condition,
      exclude = ordinance$target,
      globals = config$globals
    )
    ordinance$deps_change <- import_dependencies(
      ordinance$trigger$change,
      exclude = ordinance$target,
      globals = config$globals
    )
  }
  ordinance
}
