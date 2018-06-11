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
#' @importFrom base64url base64_urldecode base64_urlencode
#' @importFrom txtq txtq
#' @importFrom bindr populate_env
#' @importFrom CodeDepends getInputs
#' @importFrom crayon make_style
#' @importFrom digest digest
#' @importFrom dplyr bind_rows do group_by mutate n select ungroup
#' @importFrom evaluate try_capture_stack
#' @importFrom filelock lock unlock
#' @importFrom formatR tidy_source
#' @importFrom future future plan resolved value
#' @importFrom future.apply future_lapply
#' @importFrom fs dir_create file_create
#' @importFrom grDevices col2rgb rgb
#' @importFrom igraph adjacent_vertices components delete_vertices
#'   degree edge induced_subgraph is_dag make_empty_graph plot.igraph
#'   simplify subcomponent topo_sort V vertex vertex_attr
#' @importFrom knitr knit
#' @importFrom lubridate dseconds duration
#' @importFrom magrittr %>%
#' @importFrom parallel clusterCall clusterExport makePSOCKcluster
#'   mclapply parLapply stopCluster
#' @importFrom pkgconfig get_config
#' @importFrom purrr map_int
#' @importFrom R6 R6Class
#' @importFrom R.utils countLines isPackageLoaded withTimeout
#' @importFrom rlang expr exprs
#' @importFrom rprojroot find_root
#' @importFrom stats coef complete.cases lm na.omit rnorm rpois runif setNames
#' @importFrom storr encode64 storr_environment storr_rds
#' @importFrom stringi stri_extract_all_regex stri_rand_strings
#'   stri_split_fixed stri_trim_both
#' @importFrom testthat context expect_false expect_true test_dir test_that
#' @importFrom tibble as_tibble tibble
#' @importFrom tidyselect vars_select
#' @importFrom utils capture.output compareVersion head installed.packages
#'   packageVersion read.csv sessionInfo type.convert unzip write.table
#' @importFrom visNetwork toVisNetworkData visEvents visHierarchicalLayout
#'   visIgraphLayout visInteraction visLegend visNetwork visSave
#' @importFrom withr local_dir with_dir with_options with_output_sink
#'   with_preserve_seed with_seed
NULL
