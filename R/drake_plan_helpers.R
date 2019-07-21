#' @title Row-bind together drake plans
#' @description Combine drake plans together in a way that
#'   correctly fills in missing entries.
#' @export
#' @seealso [drake_plan()], [make()]
#' @param ... Workflow plan data frames (see [drake_plan()]).
#' @examples
#' # You might need to refresh your data regularly (see ?triggers).
#' download_plan <- drake_plan(
#'   data = target(
#'     command = download_data(),
#'     trigger = "always"
#'   )
#' )
#' # But if the data don't change, the analyses don't need to change.
#' analysis_plan <- drake_plan(
#'   usage = get_usage_metrics(data),
#'   topline = scrape_topline_table(data)
#' )
#' your_plan <- bind_plans(download_plan, analysis_plan)
#' your_plan
bind_plans <- function(...) {
  sanitize_plan(drake_bind_rows(...))
}
