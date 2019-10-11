create_drake_layout <- function(
  plan,
  envir = parent.frame(),
  logger,
  jobs = 1,
  trigger = drake::trigger(),
  cache = NULL
) {
  force(envir)
  import_names <- names(envir)
  config <- list(
    plan = plan,
    envir = envir,
    logger = logger,
    jobs = jobs,
    cache = cache,
    trigger = cdl_parse_trigger(trigger = trigger, envir = envir),
    allowed_globals_imports = ht_new(import_names),
    allowed_globals_targets = ht_new(c(import_names, plan$target))
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

# https://github.com/ropensci/drake/issues/887 # nolint
cdl_set_knitr_files <- function(config, layout) {
  config$logger$minor("set knitr files")
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
  config$logger$minor("get knitr hash")
  if (!config$cache$exists(key = "knitr", namespace = "memoize")) {
    return(NA_character_)
  }
  knitr_files <- config$cache$safe_get(key = "knitr", namespace = "memoize")
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
    algo = config$cache$hash_algorithm,
    serialize = FALSE
  )
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

cdl_prepare_imports <- function(config) {
  config$logger$minor("analyze environment")
  imports <- as.list(config$envir)
  cdl_unload_conflicts(
    imports = names(imports),
    targets = config$plan$target,
    envir = config$envir,
    logger = config$logger
  )
  import_names <- setdiff(names(imports), config$plan$target)
  imports[import_names]
}

cdl_unload_conflicts <- function(imports, targets, envir, logger) {
  common <- intersect(imports, targets)
  if (length(common)) {
    logger$major(
      "unload",
      "targets from environment:\n",
      multiline_message(common), sep = ""
    )
  }
  remove(list = common, envir = envir)
}

cdl_analyze_imports <- function(config, imports) {
  names <-  names(imports)
  out <- lightly_parallelize(
    X = seq_along(imports),
    FUN = function(i) {
      config$logger$minor("analyze", target = names[i])
      list(
        target = names[i],
        deps_build = cdl_import_dependencies(
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
      cdl_parse_trigger,
      envir = config$eval
    )
  }
  layout <- drake_pmap(.l = config$plan, .f = list, jobs = config$jobs)
  names(layout) <- config$plan$target
  config$default_condition_deps <- cdl_import_dependencies(
    config$trigger$condition,
    allowed_globals = config$allowed_globals_targets
  )
  config$default_change_deps <- cdl_import_dependencies(
    config$trigger$change,
    allowed_globals = config$allowed_globals_targets
  )
  out <- lightly_parallelize(
    X = layout,
    FUN = cdl_prepare_layout,
    jobs = config$jobs,
     config = config,
    ht_targets = ht_new(config$plan$target)
  )
  names(out) <- config$plan$target
  out
}

cdl_prepare_layout <- function(config, layout, ht_targets) {
  config$logger$minor("analyze", target = layout$target)
  layout$deps_build <- cdl_command_dependencies(
    command = layout$command,
    exclude = layout$target,
    allowed_globals = config$allowed_globals_targets
  )
  layout$command_standardized <- cdl_standardize_command(layout$command)
  layout$command_build <- cdl_preprocess_command(
    layout$command,
    config = config
  )
  if (is.null(layout$trigger) || all(is.na(layout$trigger))) {
    layout$trigger <- config$trigger
    layout$deps_condition <- config$default_condition_deps
    layout$deps_change <- config$default_change_deps
  } else {
    layout$deps_condition <- cdl_import_dependencies(
      layout$trigger$condition,
      exclude = layout$target,
      allowed_globals = config$allowed_globals_targets
    )
    layout$deps_change <- cdl_import_dependencies(
      layout$trigger$change,
      exclude = layout$target,
      allowed_globals = config$allowed_globals_targets
    )
  }
  for (field in c("deps_build", "deps_condition", "deps_change")) {
    layout[[field]]$memory <- ht_filter(
      ht = ht_targets,
      x = layout[[field]]$globals
    )
  }
  layout
}

cdl_import_dependencies <- function(
  expr, exclude = character(0), allowed_globals = NULL
) {
  deps <- analyze_code(
    expr = expr,
    exclude = exclude,
    allowed_globals = allowed_globals
  )
  deps$file_out <- deps$strings <- NULL
  select_nonempty(deps)
}

cdl_command_dependencies <- function(
  command,
  exclude = character(0),
  allowed_globals = NULL
) {
  if (!length(command)) {
    return()
  }
  deps <- analyze_code(
    command,
    exclude = exclude,
    allowed_globals = allowed_globals
  )
  deps$strings <- NULL
  select_nonempty(deps)
}

# Get the command ready for tidy eval prep
# and then pure eval (no side effects).
cdl_preprocess_command <- function(command, config) {
  command <- as.call(c(quote(local), command))
  # Here, we really do need expr() instead of quo().
  # `!!` needs to unquote symbols using config$eval instead of
  # the environment where the original binding took place.
  # In other words, `drake` already supplies the correct
  # evaluation environment.
  as.call(c(quote(rlang::expr), command))
}

cdl_standardize_command <- function(x) {
  if (is.expression(x) && length(x) == 1L) {
    x <- x[[1]]
  }
  look_for_ignore <- "ignore" %in% all.vars(x, functions = TRUE)
  if (look_for_ignore) {
    x <- ignore_ignore(x)
  }
  attributes(x) <- NULL
  safe_deparse(x)
}

cdl_parse_trigger <- function(trigger, envir) {
  if (is.symbol(trigger)) {
    trigger <- safe_deparse(trigger)
  }
  if (is.character(trigger)) {
    trigger <- convert_old_trigger(trigger)
    trigger <- parse(text = trigger)
  }
  eval(trigger, envir = envir)
}
