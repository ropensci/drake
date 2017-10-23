#' Drake is a workflow manager and build system,
#' a scalable solution for reproducibility and high-performance computing.
#' @docType package
#' @name drake-package
#' @aliases drake
#' @author William Michael Landau \email{will.landau@@lilly.com}
#' @examples
#' \dontrun{
#' library(drake)
#' load_basic_example()
#' make(my_plan) # Build everything.
#' make(my_plan) # Nothing is done because everything is already up to date.
#' reg2 = function(d){ # Change one of your functions.
#'   d$x3 = d$x^3
#'   lm(y ~ x3, data = d)
#' }
#' make(my_plan) # Only the pieces depending on reg2() get rebuilt.
#' readd(small) # Read/load from the cache.
#' loadd(large)
#' head(large)
#' clean() # Restart from scratch
#' make(my_plan, jobs = 2) # Distribute over 2 parallel jobs.
#' clean()
#' make(my_plan, jobs = 4, parallelism = "Makefile") # 4 parallel R sessions.
#' make(my_plan, jobs = 4, parallelism = "Makefile") # Everything up to date.
#' clean(destroy = TRUE) # Totally remove the cache.
#' unlink(c("Makefile", "report.Rmd"))
#' }
#' @references \url{https://github.com/wlandau-lilly/drake}
#' @importFrom codetools findGlobals
#' @importFrom crayon make_style
#' @importFrom digest digest
#' @importFrom eply quotes strings unquote
#' @importFrom grDevices col2rgb rgb
#' @importFrom igraph adjacent_vertices delete_vertices edge
#' is_dag make_empty_graph plot.igraph subcomponent V vertex
#' @importFrom knitr knit
#' @importFrom lubridate dseconds duration
#' @importFrom magrittr %>%
#' @importFrom parallel clusterCall clusterExport makePSOCKcluster
#' mclapply parLapply stopCluster
#' @importFrom plyr ddply dlply
#' @importFrom R.utils isPackageLoaded withTimeout
#' @importFrom rprojroot find_root
#' @importFrom stats coef complete.cases lm rnorm rpois runif setNames
#' @importFrom storr encode64 storr_rds
#' @importFrom stringi stri_extract_all_regex stri_rand_strings
#' @importFrom stringr str_split str_trim
#' @importFrom testthat context expect_false expect_true test_dir test_that
#' @importFrom utils capture.output compareVersion installed.packages
#' packageVersion read.csv sessionInfo type.convert unzip
#' @importFrom visNetwork toVisNetworkData visEvents visHierarchicalLayout
#' visIgraphLayout visInteraction visLegend visNetwork visSave
#' @importFrom withr with_dir with_options with_output_sink
#' with_preserve_seed with_seed
NULL

#' @title Pipe operator
#' @export
#' @description Pipe operator from the \code{magrittr} package
#' @param lhs left-hand side argument
#' @param rhs right-hand side argument
#' @examples
#' rnorm(2) %>%
#'   mean %>%
#'   c("third element")
`%>%` <- magrittr::`%>%`
