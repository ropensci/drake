cleaned_namespaces_ <- function(
  default = storr::storr_environment()$default_namespace
) {
  out <- c(default, "meta")
  sort(out)
}

# List the `storr` cache namespaces that store target-level information.
target_namespaces_ <- function(
  default = storr::storr_environment()$default_namespace
) {
  out <- c(
    cleaned_namespaces_(default = default),
    "progress"
  )
  sort(out)
}

list_multiple_namespaces <- function(cache, namespaces, jobs = 1) {
  out <- lightly_parallelize(
    X = namespaces,
    FUN = function(namespace) {
      cache$list(namespace = namespace)
    },
    jobs = jobs
  )
  Reduce(out, f = base::union)
}
