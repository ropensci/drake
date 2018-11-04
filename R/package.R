#' drake is a pipeline toolkit
#' (<https://github.com/pditommaso/awesome-pipeline>)
#' and a scalable, R-focused solution for reproducibility
#' and high-performance computing.
#' @docType package
#' @name drake-package
#' @aliases drake
#' @author William Michael Landau \email{will.landau@@gmail.com}
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' library(drake)
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Build everything.
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
#' clean() # Restart from scratch.
#' make(my_plan, jobs = 2) # Distribute over 2 parallel jobs.
#' clean() # Restart from scratch.
#' # Parallelize over at most 4 separate R sessions.
#' # Requires Rtools on Windows.
#' # make(my_plan, jobs = 4, parallelism = "Makefile") # nolint
#' # Everything is already up to date.
#' # make(my_plan, jobs = 4, parallelism = "Makefile") # nolint
#' clean(destroy = TRUE) # Totally remove the cache.
#' unlink("report.Rmd") # Clean up the remaining files.
#' })
#' }
#' @references <https://github.com/ropensci/drake>
#' @importFrom codetools findGlobals
#' @importFrom digest digest
#' @importFrom dplyr bind_rows do group_by n select ungroup
#' @importFrom formatR tidy_source
#' @importFrom igraph adjacent_vertices components delete_vertices
#'   degree igraph_opt igraph_options induced_subgraph is_dag
#'   make_empty_graph set_vertex_attr simplify subcomponent
#'   topo_sort V vertex_attr
#' @importFrom pkgconfig get_config
#' @importFrom purrr map_int pmap pmap_chr
#' @importFrom R6 R6Class
#' @importFrom rlang enexpr enexprs enquo expr expr_text exprs
#' @importFrom storr storr_environment storr_rds
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyselect vars_select
#' @importFrom utils capture.output compareVersion head packageVersion
#'   read.csv sessionInfo stack type.convert unzip write.table
#' @importFrom withr local_dir with_options with_output_sink with_seed
NULL
