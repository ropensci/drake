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

package_version <- function(x){
  packageVersion(sans_package(x))
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
