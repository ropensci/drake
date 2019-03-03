#' @title Return the [sessionInfo()]
#'   of the last call to [make()].
#' @description By default, session info is saved
#' during [make()] to ensure reproducibility.
#' Your loaded packages and their versions are recorded, for example.
#' @seealso [diagnose()], [cached()],
#'   [readd()], [drake_plan()], [make()]
#' @export
#' @return [sessionInfo()] of the last
#'   call to [make()]
#' @inheritParams cached
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' drake_get_session_info() # Get the cached sessionInfo() of the last make().
#' }
#' })
#' }
drake_get_session_info <- function(
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L
) {
  if (is.null(cache)) {
    stop("No drake::make() session detected.")
  }
  return(cache$get("sessionInfo", namespace = "session"))
}

drake_set_session_info <- function(
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = 1L,
  full = TRUE
) {
  if (is.null(cache)) {
    stop("No drake::make() session detected.")
  }
  if (full) {
    cache$set(
      key = "sessionInfo",
      value = utils::sessionInfo(),
      namespace = "session"
    )
  }
  cache$set(
    key = "drake_version",
    value = as.character(utils::packageVersion("drake")),
    namespace = "session"
  )
  invisible()
}

initialize_session <- function(config) {
  runtime_checks(config = config)
  config$cache$set(key = "seed", value = config$seed, namespace = "session")
  init_common_values(config$cache)
  config$eval[[drake_plan_marker]] <- config$plan
  if (config$log_progress) {
    clear_tmp_namespace(
      cache = config$cache,
      jobs = config$jobs_preprocess,
      namespace = "progress"
    )
  }
  drake_set_session_info(cache = config$cache, full = config$session_info)
  do_prework(config = config, verbose_packages = config$verbose)
  invisible()
}

conclude_session <- function(config) {
  drake_cache_log_file(
    file = config$cache_log_file,
    cache = config$cache,
    jobs = config$jobs
  )
  remove(list = names(config$eval), envir = config$eval)
  console_final_notes(config)
  invisible()
}

prompt_intv_make <- function(config) {
  show_menu <- .pkg_envir[["drake_make_menu"]] %||%
    getOption("drake_make_menu") %||%
    TRUE
  interactive() &&
    igraph::gorder(config$schedule) &&
    show_menu
}

abort_intv_make <- function(config) {
  # nocov start
  on.exit(
    assign(
      x = "drake_make_menu",
      value = FALSE,
      envir = .pkg_envir,
      inherits = FALSE
    )
  )
  title <- paste(
    paste(igraph::gorder(config$schedule), "outdated targets:"),
    multiline_message(igraph::V(config$schedule)$name),
    "\nPlease read the \"Interactive mode\" section of the make() help file.",
    "\nIn interactive mode, r_make() is more reproducible than make().",
    "This prompt only appears once per session.",
    "\nReally run make() in interactive mode?",
    sep = "\n"
  )
  out <- utils::menu(choices = c("yes", "no"), title = title)
  identical(as.integer(out), 2L)
  # nocov end
}
