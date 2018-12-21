whole_static_analysis <- function(
  plan = read_drake_plan(),
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
    envir = envir,
    verbose = verbose,
    jobs = jobs,
    cache = cache,
    console_log_file = console_log_file,
    trigger = parse_trigger(trigger = trigger, envir = envir),
    allowed_globals = sort(c(plan$target, ls(envir, all.names = TRUE)))
  )
  imports <- wsa_prepare_imports(config)
  imports_kernel <- wsa_imports_kernel(config, imports)
  import_layout <- memo_expr(
    wsa_analyze_imports(config, imports),
    config$cache,
    imports_kernel
  )
  command_layout <- memo_expr(
    wsa_analyze_commands(config),
    config$cache,
    config$plan,
    config$trigger,
    config$allowed_globals,
    import_layout
  )
  layout <- c(import_layout, command_layout)
  path_encodings <- memo_expr(
    wsa_get_path_encodings(config, layout),
    config$cache,
    layout
  )
  layout <- memo_expr(
    wsa_encode_layout_keys(
      config = config,
      layout = layout,
      path_encodings = path_encodings
    ),
    config$cache,
    layout,
    path_encodings
  )
  list(
    layout = layout,
    decode = path_encodings$decode,
    encode = path_encodings$encode
  )
}

wsa_prepare_imports <- function(config) {
  console_preprocess(text = "analyze environment", config = config)
  imports <- as.list(config$envir)
  wsa_unload_conflicts(
    imports = names(imports),
    targets = config$plan$target,
    envir = config$envir,
    verbose = config$verbose
  )
  import_names <- setdiff(names(imports), config$plan$target)
  imports[import_names]
}

wsa_unload_conflicts <- function(imports, targets, envir, verbose) {
  common <- intersect(imports, targets)
  if (verbose & length(common)) {
    message(
      "Unloading targets from environment:\n",
      multiline_message(common), sep = ""
    )
  }
  remove(list = common, envir = envir)
}

wsa_imports_kernel <- function(config, imports) {
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

wsa_analyze_imports <- function(config, imports) {
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

wsa_analyze_commands <- function(config) {
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
    FUN = wsa_prepare_layout,
    jobs = config$jobs,
    config = config
  )
  names(out) <- config$plan$target
  out
}

wsa_prepare_layout <- function(layout, config){
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

wsa_get_path_encodings <- function(config, layout) {
  console_preprocess(text = "encode file paths", config = config)
  decode <- ht_new()
  encode <- ht_new()
  decoded <- wsa_collect_paths(layout = layout)
  if (length(decoded)) {
    encoded <- reencode_path(decoded)
    names(decoded) <- encoded
    names(encoded) <- decoded
    list2env(x = as.list(decoded), envir = decode, hash = TRUE)
    list2env(x = as.list(encoded), envir = encode, hash = TRUE)

  }
  list(decode = decode, encode = encode)
}

wsa_collect_paths <- function(layout) {
  out <- lapply(
    X = layout,
    FUN = function(x) {
      out <- character(0)
      for (deps_field in c("deps_build", "deps_condition", "deps_change")) {
        for (file_field in c("file_in", "file_out", "knitr_in")) {
          out <- c(out, x[[deps_field]][[file_field]])
        }
      }
      out
    }
  )
  unique(as.character(unlist(out)))
}

wsa_encode_layout_keys <- function(config, layout, path_encodings) {
  lapply(
    X = layout,
    FUN = wsa_encode_layout_step,
    encode = path_encodings$encode
  )
}

wsa_encode_layout_step <- function(layout, encode) {
  for (deps_field in c("deps_build", "deps_condition", "deps_change")) {
    for (file_field in c("file_in", "file_out", "knitr_in")) {
      layout[[deps_field]][[file_field]] <- wsa_encode_path_vector(
        x = layout[[deps_field]][[file_field]],
        encode = encode
      )
    }
    layout[[deps_field]]$namespaced <- encode_namespaced(
      layout[[deps_field]]$namespaced
    )
  }
  layout
}

wsa_encode_path_vector <- function(x, encode) {
  vapply(
    X = x,
    FUN = function(y) {
      encode[[y]]
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}
