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
  args <- list(
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
  imports <- cds_prepare_imports(args)
  imports_kernel <- cds_imports_kernel(args, imports)
  import_spec <- cds_analyze_imports(args, imports)
  knitr_hash <- cds_get_knitr_hash(args)
  command_spec <- cds_analyze_commands(args)
  cds_set_knitr_files(args = args, spec = command_spec)
  out <- c(import_spec, command_spec)
  list2env(out, parent = emptyenv(), hash = TRUE)
}

# https://github.com/ropensci/drake/issues/887 # nolint
cds_set_knitr_files <- function(args, spec) {
  args$logger$disk("set knitr files")
  knitr_files <- lightly_parallelize(
    X = spec,
    FUN = function(x) {
      x$deps_build$knitr_in
    },
    jobs = args$jobs
  )
  knitr_files <- sort(unique(as.character(unlist(knitr_files))))
  args$cache$set(
    key = "knitr",
    value = knitr_files,
    namespace = "memoize"
  )
}

cds_get_knitr_hash <- function(args, spec) {
  args$logger$disk("get knitr hash")
  if (!args$cache$exists(key = "knitr", namespace = "memoize")) {
    out <- args$cache$digest("", serialize = FALSE)
    return(out)
  }
  knitr_files <- args$cache$safe_get(key = "knitr", namespace = "memoize")
  knitr_hashes <- lightly_parallelize(
    X = knitr_files,
    FUN = static_storage_hash,
    jobs = args$jobs,
    config = args
  )
  knitr_hashes <- as.character(unlist(knitr_hashes))
  knitr_hashes <- paste0(knitr_hashes, collapse = "")
  args$cache$digest(knitr_hashes, serialize = FALSE)
}

cds_imports_kernel <- function(args, imports) {
  out <- lightly_parallelize(
    X = imports,
    FUN = safe_deparse_function,
    jobs = args$jobs
  )
  names(out) <- names(imports)
  out[sort(as.character(names(out)))]
}

cds_prepare_imports <- function(args) {
  args$logger$disk("analyze environment")
  imports <- as.list(args$envir)
  cds_unload_conflicts(
    imports = names(imports),
    targets = args$plan$target,
    envir = args$envir,
    logger = args$logger
  )
  import_names <- setdiff(names(imports), args$plan$target)
  imports[import_names]
}

cds_unload_conflicts <- function(imports, targets, envir, logger) {
  common <- intersect(imports, targets)
  if (length(common)) {
    logger$term("unloading", length(common), "targets from environment")
  }
  remove(list = as.character(common), envir = envir)
}

cds_analyze_imports <- function(args, imports) {
  args$logger$disk("analyze imports")
  names <-  names(imports)
  out <- lightly_parallelize(
    X = seq_along(imports),
    FUN = cdl_analyze_import,
    jobs = args$jobs,
    imports = imports,
    names = names,
    args = args
  )
  names(out) <- names
  out
}

cdl_analyze_import <- function(index, imports, names, args) {
  name <- names[index]
  spec <- list(
    target = name,
    imported = TRUE,
    deps_build = cds_import_dependencies(
      expr = imports[[index]],
      exclude = name,
      restrict = args$ht_imports
    )
  )
  as_drake_spec(spec)
}

cds_analyze_commands <- function(args) {
  args$logger$disk("analyze commands")
  args$plan$imported <- FALSE
  if ("trigger" %in% colnames(args$plan)) {
    args$plan$trigger <- lapply(
      args$plan$trigger,
      cds_parse_trigger,
      envir = args$envir_targets
    )
  }
  spec <- drake_pmap(.l = args$plan, .f = list, jobs = args$jobs)
  names(spec) <- args$plan$target
  args$default_condition_deps <- cds_import_dependencies(
    args$trigger$condition,
    restrict = args$ht_globals
  )
  args$default_change_deps <- cds_import_dependencies(
    args$trigger$change,
    restrict = args$ht_globals
  )
  out <- lightly_parallelize(
    X = spec,
    FUN = cds_prepare_spec,
    jobs = args$jobs,
    args = args
  )
  names(out) <- args$plan$target
  out
}

cds_prepare_spec <- function(args, spec) {
  target <- spec$target
  spec$dynamic <- as_dynamic(spec$dynamic)
  spec$deps_build <- cds_command_dependencies(
    command = spec$command,
    exclude = spec$target,
    restrict = args$ht_globals
  )
  spec$deps_dynamic <- cds_dynamic_deps(
    spec$dynamic,
    spec$target,
    args
  )
  spec$deps_dynamic_trace <- cds_dynamic_trace(spec$dynamic, args)
  cds_assert_trace(spec$dynamic, spec)
  spec$command_standardized <- cds_standardize_command(spec$command)
  if (inherits(spec$dynamic, "dynamic")) {
    dynamic_command <- cds_std_dyn_cmd(spec$dynamic)
    spec$command_standardized <- paste(
      spec$command_standardized,
      dynamic_command
    )
    cds_exclude_dynamic_file_dsl(spec, field = "file_out")
    cds_exclude_dynamic_file_dsl(spec, field = "knitr_in")
  }
  spec$command_build <- cds_preprocess_command(
    spec$command,
    args = args
  )
  if (is.null(spec$trigger) || all(is.na(spec$trigger))) {
    spec$trigger <- args$trigger
    spec$deps_condition <- args$default_condition_deps
    spec$deps_change <- args$default_change_deps
  } else {
    spec$deps_condition <- cds_import_dependencies(
      spec$trigger$condition,
      exclude = spec$target,
      restrict = args$ht_globals
    )
    spec$deps_change <- cds_import_dependencies(
      spec$trigger$change,
      exclude = spec$target,
      restrict = args$ht_globals
    )
  }
  cds_no_dynamic_triggers(spec)
  for (field in c("deps_build", "deps_condition", "deps_change")) {
    spec[[field]]$memory <- ht_filter(
      ht = args$ht_targets,
      x = spec[[field]]$globals
    )
  }
  spec$deps_build$memory <- base::union(
    spec$deps_build$memory,
    spec$deps_dynamic_trace
  )
  as_drake_spec(spec)
}

cds_exclude_dynamic_file_dsl <- function(spec, field) {
  if (length(spec$deps_build[[field]])) {
    stop0(
      field,
      "() in dynamic targets is illegal. Target: ",
      spec$target
    )
  }
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
  stop0(
    "Dynamic grouping variables are forbidden in the condition ",
    "and change triggers. Found dynamic grouping variables for target ",
    target, ":\n", multiline_message(common)
  )
}

cds_import_dependencies <- function(
  expr, exclude = character(0), restrict = NULL
) {
  deps <- drake_deps(expr = expr, exclude = exclude, restrict = restrict)
  deps$file_out <- deps$strings <- character(0)
  deps
}

cds_command_dependencies <- function(
  command,
  exclude = character(0),
  restrict = NULL
) {
  deps <- drake_deps(command, exclude = exclude, restrict = restrict)
  deps$strings <- character(0)
  deps
}

cds_dynamic_deps <- function(dynamic, target, args) {
  UseMethod("cds_dynamic_deps")
}

#' @export
cds_dynamic_deps.dynamic <- function(dynamic, target, args) {
  dynamic$.trace <- NULL
  out <- ht_filter(args$ht_globals, all.vars(dynamic))
  if (!length(out)) {
    stop0(
      "no admissible grouping variables for dynamic target ",
      target
    )
  }
  out
}

#' @export
cds_dynamic_deps.default <- function(dynamic, target, args) {
  character(0)
}

cds_dynamic_trace <- function(dynamic, args) {
  UseMethod("cds_dynamic_trace")
}

#' @export
cds_dynamic_trace.dynamic <- function(dynamic, args) {
  all.vars(dynamic$.trace)
}

#' @export
cds_dynamic_trace.default <- function(dynamic, args) {
  character(0)
}

cds_assert_trace <- function(dynamic, spec) {
  UseMethod("cds_assert_trace")
}

#' @export
cds_assert_trace.group <- function(dynamic, spec) {
  bad <- setdiff(spec$deps_dynamic_trace, spec$deps_dynamic)
  if (!length(bad)) {
    return()
  }
  stop0(
    "in dynamic group(), ",
    "the only legal dynamic trace variable ",
    "is the one you select with `.by`. ",
    "illegal dynamic trace variables for target ",
    spec$target,
    ":\n",
    multiline_message(bad)
  )
}

#' @export
cds_assert_trace.dynamic <- function(dynamic, spec) {
  bad <- setdiff(spec$deps_dynamic_trace, spec$deps_dynamic)
  if (!length(bad)) {
    return()
  }
  stop0(
    "in map() and cross(), ",
    "all dynamic trace variables must be ",
    "existing grouping variables, e.g. map(x, .trace = x) ",
    "and not map(x, .trace = y). ",
    "illegal dynamic trace variables for target ",
    spec$target,
    ":\n",
    multiline_message(bad)
  )
}

#' @export
cds_assert_trace.default <- function(dynamic, spec) {
  character(0)
}

# Get the command ready for tidy eval prep
# and then pure eval (no side effects).
cds_preprocess_command <- function(command, args) {
  command <- as.call(c(quote(local), command))
  # Here, we really do need expr() instead of quo().
  # `!!` needs to unquote symbols using args$envir_targets instead of
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
  # TODO: the mention of trace is a bug, but fixing it
  # will invalidate everyone's sub-targets. Wait to fix it
  # until drake 8.0.0.
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
