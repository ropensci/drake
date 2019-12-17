create_drake_spec <- function(
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
    trigger = cds_parse_trigger(trigger = trigger, envir = envir),
    ht_targets = ht_new(plan$target),
    ht_imports = ht_new(import_names),
    ht_globals = ht_new(c(import_names, plan$target))
  )
  imports <- cds_prepare_imports(config)
  imports_kernel <- cds_imports_kernel(config, imports)
  import_spec <- memo_expr(
    cds_analyze_imports(config, imports),
    config$cache,
    imports_kernel
  )
  knitr_hash <- cds_get_knitr_hash(config)
  command_spec <- memo_expr(
    cds_analyze_commands(config),
    config$cache,
    config$plan,
    config$trigger,
    import_spec,
    imports_kernel,
    knitr_hash
  )
  cds_set_knitr_files(config = config, spec = command_spec)
  out <- c(import_spec, command_spec)
  list2env(out, parent = emptyenv(), hash = TRUE)
}

# https://github.com/ropensci/drake/issues/887 # nolint
cds_set_knitr_files <- function(config, spec) {
  config$logger$minor("set knitr files")
  knitr_files <- lightly_parallelize(
    X = spec,
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

cds_get_knitr_hash <- function(config, spec) {
  config$logger$minor("get knitr hash")
  if (!config$cache$exists(key = "knitr", namespace = "memoize")) {
    out <- config$cache$digest("", serialize = FALSE)
    return(out)
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
  config$cache$digest(knitr_hashes, serialize = FALSE)
}

cds_imports_kernel <- function(config, imports) {
  out <- lightly_parallelize(
    X = imports,
    FUN = safe_deparse_function,
    jobs = config$jobs
  )
  names(out) <- names(imports)
  out[sort(as.character(names(out)))]
}

cds_prepare_imports <- function(config) {
  config$logger$minor("analyze environment")
  imports <- as.list(config$envir)
  cds_unload_conflicts(
    imports = names(imports),
    targets = config$plan$target,
    envir = config$envir,
    logger = config$logger
  )
  import_names <- setdiff(names(imports), config$plan$target)
  imports[import_names]
}

cds_unload_conflicts <- function(imports, targets, envir, logger) {
  common <- intersect(imports, targets)
  if (length(common)) {
    logger$major(
      "unload",
      "targets from environment:\n",
      multiline_message(common),
      sep = ""
    )
  }
  remove(list = common, envir = envir)
}

cds_analyze_imports <- function(config, imports) {
  config$logger$minor("analyze imports")
  names <-  names(imports)
  out <- lightly_parallelize(
    X = seq_along(imports),
    FUN = cdl_analyze_import,
    jobs = config$jobs,
    imports = imports,
    names = names,
    config = config
  )
  names(out) <- names
  out
}

cdl_analyze_import <- function(index, imports, names, config) {
  name <- names[index]
  spec <- list(
    target = name,
    imported = TRUE,
    deps_build = cds_import_dependencies(
      expr = imports[[index]],
      exclude = name,
      allowed_globals = config$ht_imports
    )
  )
  as_drake_spec(spec)
}

cds_analyze_commands <- function(config) {
  config$logger$minor("analyze commands")
  config$plan$imported <- FALSE
  if ("trigger" %in% colnames(config$plan)) {
    config$plan$trigger <- lapply(
      config$plan$trigger,
      cds_parse_trigger,
      envir = config$envir_targets
    )
  }
  spec <- drake_pmap(.l = config$plan, .f = list, jobs = config$jobs)
  names(spec) <- config$plan$target
  config$default_condition_deps <- cds_import_dependencies(
    config$trigger$condition,
    allowed_globals = config$ht_globals
  )
  config$default_change_deps <- cds_import_dependencies(
    config$trigger$change,
    allowed_globals = config$ht_globals
  )
  out <- lightly_parallelize(
    X = spec,
    FUN = cds_prepare_spec,
    jobs = config$jobs,
    config = config
  )
  names(out) <- config$plan$target
  out
}

cds_prepare_spec <- function(config, spec) {
  target <- spec$target
  spec$dynamic <- as_dynamic(spec$dynamic)
  spec$deps_build <- cds_command_dependencies(
    command = spec$command,
    exclude = spec$target,
    allowed_globals = config$ht_globals
  )
  spec$deps_dynamic <- cds_dynamic_deps(
    spec$dynamic,
    spec$target,
    config
  )
  spec$deps_dynamic_trace <- cds_dynamic_trace(spec$dynamic, config)
  cds_assert_trace(spec$dynamic, spec)
  spec$command_standardized <- cds_standardize_command(spec$command)
  if (inherits(spec$dynamic, "dynamic")) {
    dynamic_command <- cds_std_dyn_cmd(spec$dynamic)
    spec$command_standardized <- paste(
      spec$command_standardized,
      dynamic_command
    )
  }
  spec$command_build <- cds_preprocess_command(
    spec$command,
    config = config
  )
  if (is.null(spec$trigger) || all(is.na(spec$trigger))) {
    spec$trigger <- config$trigger
    spec$deps_condition <- config$default_condition_deps
    spec$deps_change <- config$default_change_deps
  } else {
    spec$deps_condition <- cds_import_dependencies(
      spec$trigger$condition,
      exclude = spec$target,
      allowed_globals = config$ht_globals
    )
    spec$deps_change <- cds_import_dependencies(
      spec$trigger$change,
      exclude = spec$target,
      allowed_globals = config$ht_globals
    )
  }
  cds_no_dynamic_triggers(spec)
  for (field in c("deps_build", "deps_condition", "deps_change")) {
    spec[[field]]$memory <- ht_filter(
      ht = config$ht_targets,
      x = spec[[field]]$globals
    )
  }
  spec$deps_build$memory <- base::union(
    spec$deps_build$memory,
    spec$deps_dynamic_trace
  )
  as_drake_spec(spec)
}

as_drake_spec <- function(spec) {
  class(spec) <- c("drake_spec", "drake")
  spec
}

#' @export
print.drake_spec <- function(x, ...) {
  type <- ifelse(x$imported, "import", "target")
  cat("drake workflow specification of", type, x$target, "\n")
  utils::str(x, no.list = TRUE)
}

cds_no_dynamic_triggers <- function(spec) {
  cds_no_dynamic_triggers_impl(
    spec$target,
    spec$deps_dynamic,
    unlist(spec$deps_condition)
  )
  cds_no_dynamic_triggers_impl(
    spec$target,
    spec$deps_dynamic,
    unlist(spec$deps_change)
  )
}

cds_no_dynamic_triggers_impl <- function(target, deps_dynamic, deps_trigger) {
  common <- intersect(deps_dynamic, deps_trigger)
  if (!length(common)) {
    return()
  }
  stop(
    "Dynamic grouping variables are forbidden in the condition ",
    "and change triggers. For target ", target, ", found dynamic ",
    "grouping variables:\n", multiline_message(common),
    call. = FALSE
  )
}

cds_import_dependencies <- function(
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

cds_command_dependencies <- function(
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

cds_dynamic_deps <- function(dynamic, target, config) {
  UseMethod("cds_dynamic_deps")
}

cds_dynamic_deps.dynamic <- function(dynamic, target, config) {
  dynamic$.trace <- NULL
  out <- ht_filter(config$ht_globals, all.vars(dynamic))
  if (!length(out)) {
    stop(
      "no admissible grouping variables for dynamic target ",
      target,
      call. = FALSE
    )
  }
  out
}

cds_dynamic_deps.default <- function(dynamic, target, config) {
  character(0)
}

cds_dynamic_trace <- function(dynamic, config) {
  UseMethod("cds_dynamic_trace")
}

cds_dynamic_trace.dynamic <- function(dynamic, config) {
  all.vars(dynamic$.trace)
}

cds_dynamic_trace.default <- function(dynamic, config) {
  character(0)
}

cds_assert_trace <- function(dynamic, spec) {
  UseMethod("cds_assert_trace")
}

cds_assert_trace.group <- function(dynamic, spec) {
  bad <- setdiff(spec$deps_dynamic_trace, spec$deps_dynamic)
  if (!length(bad)) {
    return()
  }
  stop(
    "in dynamic group(), ",
    "the only legal dynamic trace variable ",
    "is the one you select with `.by`. ",
    "illegal dynamic trace variables for target ",
    spec$target,
    ":\n",
    multiline_message(bad),
    call. = FALSE
  )
}

cds_assert_trace.dynamic <- function(dynamic, spec) {
  bad <- setdiff(spec$deps_dynamic_trace, spec$deps_dynamic)
  if (!length(bad)) {
    return()
  }
  stop(
    "in map() and cross(), ",
    "all dynamic trace variables must be ",
    "existing grouping variables, e.g. map(x, .trace = x) ",
    "and not map(x, .trace = y). ",
    "illegal dynamic trace variables for target ",
    spec$target,
    ":\n",
    multiline_message(bad),
    call. = FALSE
  )
}

cds_assert_trace.default <- function(dynamic, spec) {
  character(0)
}

# Get the command ready for tidy eval prep
# and then pure eval (no side effects).
cds_preprocess_command <- function(command, config) {
  command <- as.call(c(quote(local), command))
  # Here, we really do need expr() instead of quo().
  # `!!` needs to unquote symbols using config$envir_targets instead of
  # the environment where the original binding took place.
  # In other words, `drake` already supplies the correct
  # evaluation environment.
  as.call(c(quote(rlang::expr), command))
}

cds_standardize_command <- function(x) {
  if (is.expression(x) && length(x) == 1L) {
    x <- x[[1]]
  }
  look_for_ignore <- "ignore" %in% all.vars(x, functions = TRUE)
  if (look_for_ignore) {
    x <- ignore_ignore(x)
  }
  attributes(x) <- NULL
  safe_deparse(x, backtick = TRUE)
}

cds_std_dyn_cmd <- function(x) {
  transform <- class(x)
  vars <- sort(all.vars(x))
  by <- as.character(x$.by)
  paste(c(transform, vars, by, trace), collapse = " ")
}

cds_parse_trigger <- function(trigger, envir) {
  if (is.symbol(trigger)) {
    trigger <- safe_deparse(trigger, backtick = FALSE)
  }
  if (is.character(trigger)) {
    trigger <- convert_old_trigger(trigger)
    trigger <- parse(text = trigger)
  }
  eval(trigger, envir = envir)
}
