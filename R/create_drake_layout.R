create_drake_layout <- function(
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
    allowed_globals = sort(c(plan$target, ls(envir = envir, all.names = TRUE)))
  )
  imports <- cdl_prepare_imports(config)
  imports_kernel <- cdl_imports_kernel(config, imports)
  import_layout <- memo_expr(
    cdl_analyze_imports(config, imports),
    config$cache,
    imports_kernel
  )
  command_layout <- memo_expr(
    cdl_analyze_commands(config),
    config$cache,
    config$plan,
    config$trigger,
    config$allowed_globals,
    import_layout
  )
  c(import_layout, command_layout)
}

cdl_prepare_imports <- function(config) {
  console_preprocess(text = "analyze environment", config = config)
  imports <- as.list(config$envir)
  cdl_unload_conflicts(
    imports = names(imports),
    targets = config$plan$target,
    envir = config$envir,
    verbose = config$verbose
  )
  import_names <- setdiff(names(imports), config$targets)
  imports[import_names]
}

cdl_unload_conflicts <- function(imports, targets, envir, verbose) {
  common <- intersect(imports, targets)
  if (verbose & length(common)) {
    message(
      "Unloading targets from environment:\n",
      multiline_message(common), sep = ""
    )
  }
  remove(list = common, envir = envir)
}

cdl_imports_kernel <- function(config, imports) {
  out <- lightly_parallelize(
    X = imports,
    FUN = function(x) {
      if (is.function(x)) {
        x <- deparse(x)
      }
      x
    },
    jobs = config$jobs
  )
  names(out) <- names(imports)
  out[sort(names(out) %||% logical(0))]
}

cdl_analyze_imports <- function(config, imports) {
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
          allowed_globals = config$allowed_globals
        ),
        imported = TRUE
      )
    },
    jobs = config$jobs
  )
  names(out) <- names(imports)
  out
}

cdl_analyze_commands <- function(config) {
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
      envir = config$eval
    )
  }
  layout <- drake_pmap(.l = config$plan, .f = list, jobs = config$jobs)
  names(layout) <- config$plan$target
  config$default_condition_deps <- import_dependencies(
    config$trigger$condition,
    allowed_globals = config$allowed_globals
  )
  config$default_change_deps <- import_dependencies(
    config$trigger$change,
    allowed_globals = config$allowed_globals
  )
  out <- lightly_parallelize(
    X = layout,
    FUN = cdl_prepare_layout,
    jobs = config$jobs,
    config = config
  )
  names(out) <- config$plan$target
  out
}

cdl_prepare_layout <- function(layout, config){
  layout$deps_build <- command_dependencies(
    command = layout$command,
    exclude = layout$target,
    allowed_globals = config$allowed_globals
  )
  layout$command_standardized <- standardize_command(layout$command)
  layout$command_build <- preprocess_command(
    layout$command,
    config = config
  )
  if (is.null(layout$trigger) || all(is.na(layout$trigger))){
    layout$trigger <- config$trigger
    layout$deps_condition <- config$default_condition_deps
    layout$deps_change <- config$default_change_deps
  } else {
    layout$deps_condition <- import_dependencies(
      layout$trigger$condition,
      exclude = layout$target,
      allowed_globals = config$allowed_globals
    )
    layout$deps_change <- import_dependencies(
      layout$trigger$change,
      exclude = layout$target,
      allowed_globals = config$allowed_globals
    )
  }
  layout
}
