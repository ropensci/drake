#' Drake is a pipeline toolkit
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
#' reg2 = function(d){ # Change one of your functions.
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
#' @importFrom dplyr bind_rows do group_by mutate n select ungroup
#' @importFrom evaluate try_capture_stack
#' @importFrom formatR tidy_source
#' @importFrom fs dir_create file_create path_ext path_ext_remove
#' @importFrom igraph adjacent_vertices components delete_vertices
#'   degree edge edges igraph_opt igraph_options induced_subgraph is_dag
#'   make_empty_graph plot.igraph set_vertex_attr simplify subcomponent
#'   topo_sort V vertex vertex_attr
#' @importFrom magrittr %>%
#' @importFrom parallel clusterCall clusterExport makePSOCKcluster
#'   mclapply parLapply stopCluster
#' @importFrom pkgconfig get_config
#' @importFrom purrr map_int pmap
#' @importFrom R6 R6Class
#' @importFrom R.utils countLines isPackageLoaded withTimeout
#' @importFrom rlang expr expr_text exprs
#' @importFrom stats coef complete.cases lm na.omit rnorm rpois runif setNames
#' @importFrom storr storr_environment storr_rds
#' @importFrom stringi stri_extract_all_regex
#'   stri_split_fixed stri_trim_both
#' @importFrom tibble as_tibble new_tibble tibble
#' @importFrom tidyselect vars_select
#' @importFrom utils capture.output compareVersion head installed.packages
#'   packageVersion read.csv sessionInfo stack type.convert unzip write.table
#' @importFrom withr local_dir with_dir with_options with_output_sink
#'   with_preserve_seed with_seed
NULL
