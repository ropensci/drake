as_package <- function(x){
  paste0("package:", x)
}

is_package <- function(x){
  grepl("^package:", x)
}

sans_package <- function(x){
  gsub("^package:", "", x)
}

is_in_package <- function(funct){
  isNamespace(environment(funct))
}

is_installed_package <- function(x, config){
  is_package(x) &
    sans_package(x) %in% config$installed_packages
}

#' @title Function package_version
#' @export
#' @description Packages are formally imported into drake workflows,
#' and they serve as dependencies of package functions.
#' Specifically, the version of each package is tracked.
#' For base packages like \code{base} and \code{stats},
#' the version of R itself is tracked. Here, the version string
#' is used, so different snapshots of R-devel have different versions.
#' @param x name of the package. Could have \code{"package:"} in front
#' of it or not.
#' @examples
#' package_version("base")
#' package_version("stats")
#' package_version("package:stats")
#' \dontrun{
#' load_basic_example()
#' plot_graph(my_plan) # Hover over the package nodes on the left.
#' }
package_version <- function(x){
  pkg <- sans_package(x)
  if (pkg %in% utils::sessionInfo()$basePkgs) {
    R.version$version.string
  } else {
    utils::packageVersion(pkg)
  }
}

description_path <- function(x){
  system.file("DESCRIPTION", package = sans_package(x),
    mustWork = TRUE) %>%
    as_file
}

package_of_function <- function(funct) {
  env <- environment(funct)
  if (isNamespace(env)) {
    getNamespaceName(env) %>%
      as_package
  } else {
    character(0)
  }
}

# package_of_name("knitr::knit") should return "package:knitr" # nolint
package_of_name <- function(x){
  funct <- tryCatch(
    flexible_get(x),
    error = function(e) {
      NULL
    }
  )
  package_of_function(funct)
}
