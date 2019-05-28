create_drake_layout <- function(
  plan,
  envir = parent.frame(),
  verbose = 1L,
  jobs = 1,
  console_log_file = NULL,
  trigger = drake::trigger(),
  cache = NULL
) {
  force(envir)
  import_names <- names(envir)
  config <- list(
    plan = plan,
    envir = envir,
    verbose = verbose,
    jobs = jobs,
    cache = cache,
    console_log_file = console_log_file,
    trigger = parse_trigger(trigger = trigger, envir = envir),
    allowed_globals_imports = ht_new(import_names),
    allowed_globals_targets = ht_new(c(import_names, plan$target)),
    ht_targets = ht_new(plan$target)
  )
  imports <- cdl_prepare_imports(config)
  imports_kernel <- cdl_imports_kernel(config, imports)
  import_layout <- memo_expr(
    cdl_analyze_imports(config, imports),
    config$cache,
    imports_kernel
  )
  knitr_hash <- cdl_get_knitr_hash(config)
  command_layout <- memo_expr(
    cdl_analyze_commands(config),
    config$cache,
    config$plan,
    config$trigger,
    import_layout,
    imports_kernel,
    knitr_hash
  )
  cdl_set_knitr_files(config = config, layout = command_layout)
  c(import_layout, command_layout)
}

cdl_prepare_imports <- function(config) {
  log_msg("analyze environment", config = config)
  imports <- as.list(config$envir)
  cdl_unload_conflicts(
    imports = names(imports),
    targets = config$plan$target,
    envir = config$envir,
    verbose = config$verbose
  )
  import_names <- setdiff(names(imports), config$plan$target)
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
        x <- safe_deparse(x)
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
  out <- lightly_parallelize(
    X = seq_along(imports),
    FUN = function(i) {
      log_msg("analyze", names[i], config = config)
      list(
        target = names[i],
        deps_build = import_dependencies(
          expr = imports[[i]],
          exclude = names(imports)[[i]],
          allowed_globals = config$allowed_globals_imports
        ),
        imported = TRUE
      )
    },
    jobs = config$jobs
  )
  names(out) <- names
  out
}

cdl_analyze_commands <- function(config) {
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
    allowed_globals = config$allowed_globals_targets
  )
  config$default_change_deps <- import_dependencies(
    config$trigger$change,
    allowed_globals = config$allowed_globals_targets
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

cdl_prepare_layout <- function(config, layout){
  log_msg("analyze", layout$target, config = config)
  layout$deps_build <- command_dependencies(
    command = layout$command,
    exclude = layout$target,
    allowed_globals = config$allowed_globals_targets
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
      allowed_globals = config$allowed_globals_targets
    )
    layout$deps_change <- import_dependencies(
      layout$trigger$change,
      exclude = layout$target,
      allowed_globals = config$allowed_globals_targets
    )
  }
  for (field in c("deps_build", "deps_condition", "deps_change")) {
    layout[[field]]$memory <- ht_filter(
      ht = config$ht_targets,
      x = layout[[field]]$globals
    )
  }
  layout
}

# https://github.com/ropensci/drake/issues/887 # nolint
cdl_set_knitr_files <- function(config, layout) {
  log_msg("set knitr files", config = config)
  knitr_files <- lightly_parallelize(
    X = layout,
    FUN = function(x) {
      x$deps_build$knitr_in
    },
    jobs = config$jobs
  )
  knitr_files <- sort(unique(as.character(unlist(knitr_files))))
  config$cache$set(
    key = "knitr",
    value = knitr_files,
    namespace = "memoize"
  )
}

cdl_get_knitr_hash <- function(config, layout) {
  log_msg("get knitr hash", config = config)
  if (!config$cache$exists(key = "knitr", namespace = "memoize")) {
    return(NA_character_)
  }
  knitr_files <- safe_get(
    key = "knitr",
    namespace = "memoize",
    config = config
  )
  knitr_hashes <- lightly_parallelize(
    X = knitr_files,
    FUN = storage_hash,
    jobs = config$jobs,
    config = config
  )
  knitr_hashes <- as.character(unlist(knitr_hashes))
  knitr_hashes <- paste0(knitr_hashes, collapse = "")
  digest::digest(
    knitr_hashes,
    algo = config$cache$driver$hash_algorithm,
    serialize = FALSE
  )
}
