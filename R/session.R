#' @title Return the [sessionInfo()]
#'   of the last call to [make()].
#' @description By default, session info is saved
#' during [make()] to ensure reproducibility.
#' Your loaded packages and their versions are recorded, for example.
#' @seealso [diagnose()], [built()], [imported()],
#'   [readd()], [drake_plan()], [make()]
#' @export
#' @return [sessionInfo()] of the last
#'   call to [make()]
#' @inheritParams cached
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the project, build the targets.
#' drake_get_session_info() # Get the cached sessionInfo() of the last make().
#' })
#' }
drake_get_session_info <- function(
  path = getwd(),
  search = TRUE,
  cache = drake::get_cache(path = path, search = search, verbose = verbose),
  verbose = drake::default_verbose()
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
  verbose = drake::default_verbose()
) {
  if (is.null(cache)) {
    stop("No drake::make() session detected.")
  }
  cache$set(
    key = "sessionInfo",
    value = sessionInfo(),
    namespace = "session"
  )
  invisible()
}

initialize_session <- function(config) {
  init_common_values(config$cache)
  mark_envir(config$eval)
  if (config$log_progress) {
    clear_tmp_namespace(
      cache = config$cache,
      jobs = imports_setting(config$jobs),
      namespace = "progress"
    )
  }
  for (namespace in c("attempt", "session")) {
    clear_tmp_namespace(
      cache = config$cache,
      jobs = imports_setting(config$jobs),
      namespace = namespace
    )
  }
  if (config$session_info) {
    drake_set_session_info(cache = config$cache)
  }
}
