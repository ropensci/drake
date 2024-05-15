#' drake: A pipeline toolkit for reproducible computation at scale.
#' @description drake is a pipeline toolkit
#' (`https://github.com/pditommaso/awesome-pipeline`)
#' and a scalable, R-focused solution for reproducibility
#' and high-performance computing.
#' @name drake-package
#' @aliases drake
#' @author William Michael Landau \email{will.landau@@gmail.com}
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' library(drake)
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Build everything.
#' plot(my_plan) # fast call to vis_drake_graph()
#' make(my_plan) # Nothing is done because everything is already up to date.
#' reg2 = function(d) { # Change one of your functions.
#'   d$x3 = d$x^3
#'   lm(y ~ x3, data = d)
#' }
#' make(my_plan) # Only the pieces depending on reg2() get rebuilt.
#' # Write a flat text log file this time.
#' make(my_plan, cache_log_file = TRUE)
#' # Read/load from the cache.
#' readd(small)
#' loadd(large)
#' head(large)
#' }
#' # Dynamic branching
#' # Get the mean mpg for each cyl in the mtcars dataset.
#' plan <- drake_plan(
#'   raw = mtcars,
#'   group_index = raw$cyl,
#'   munged = target(raw[, c("mpg", "cyl")], dynamic = map(raw)),
#'   mean_mpg_by_cyl = target(
#'     data.frame(mpg = mean(munged$mpg), cyl = munged$cyl[1]),
#'     dynamic = group(munged, .by = group_index)
#'   )
#' )
#' make(plan)
#' readd(mean_mpg_by_cyl)
#' })
#' }
#' @references `https://github.com/ropensci/drake`
#' @importFrom base64url base32_decode base32_encode
#' @importFrom digest getVDigest
#' @importFrom igraph adjacent_vertices as_ids components delete_vertices
#'   degree gorder graph_from_adjacency_matrix igraph_opt igraph_options
#'   induced_subgraph is_dag make_empty_graph make_ego_graph set_vertex_attr
#'   simplify topo_sort union V vertex_attr
#' @importFrom methods new setRefClass
#' @importFrom parallel mclapply
#' @importFrom rlang dots_list enquo eval_tidy expr quo_squash quos trace_back
#' @importFrom storr storr_environment storr_rds
#' @importFrom tidyselect all_of any_of contains ends_with everything
#'   last_col matches num_range one_of starts_with
#' @importFrom txtq txtq
#' @importFrom utils capture.output compareVersion head menu packageVersion
#'   read.csv sessionInfo str type.convert unzip write.table
#' @importFrom vctrs field new_rcrd vec_assert vec_c vec_slice
NULL
