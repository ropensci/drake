#' @title Rename targets without having to rebuild them
#' @description Changes the names of already-built targets
#'   in the cache.
#' @details Behind the scenes, [rename_targets()]
#'   registers new names for targets in the cache. Remarks:
#'   - You also need to change the target names
#'     in your [drake_plan()] as appropriate.
#'   - Any name change will invalidate downstream targets that depend
#'     on the original names.
#'   - The old names are still valid until you change them in the plan
#'     and call [make()].
#'   - The underlying data is not duplicated.
#'     This is because `drake`'s cache is powered by
#'     [`storr`](https://github.com/richfitz/storr), which allows
#'     multiple keys for each data value.
#' @export
#' @inheritParams readd
#' @param from Character vector of original target names.
#' @param to Character vector of new target names. Must be the same length
#'   as `from`.
#' @examples
#' \dontrun{
#' isolate_example("Let's rename some targets.", {
#' # Let's start with this plan.
#' plan <- drake_plan(
#'   temp = target(
#'     matrix(runif(10 ^ 3) + offset, ncol = 5),
#'     transform = map(offset = c(2, 5))
#'   )
#' )
#' make(plan)
#'
#' # Later on, we add two more targets.
#' plan <- drake_plan(
#'   temp = target(
#'     matrix(runif(10 ^ 3) + offset, ncol = ncol),
#'     transform = cross(
#'       ncol = c(5, 2),
#'       offset = c(2, 5)
#'     )
#'   )
#' )
#'
#' # But then our original targets have different names,
#' # and they are no longer up to date.
#' config <- drake_config(plan)
#' outdated(config)
#'
#' # Let's rename those old targets.
#' rename_targets(
#'   from = c("temp_2", "temp_5"),
#'   to = c("temp_5_2", "temp_5_5")
#' )
#'
#' # Now, those old targets are up to date and make() skips them.
#' outdated(config)
#' make(plan)
#' })
#' }
rename_targets <- function(
  from,
  to,
  cache = drake::drake_cache(path = path, verbose = verbose),
  path = NULL,
  verbose = 1L
) {
  rename_targets_(from = from, to = to, cache = cache)
  invisible()
}

rename_targets_ <- Vectorize(
  function(from, to, cache) {
    for (ns in c("objects", "meta")) {
      cache$duplicate(from, to, namespace = ns)
      cache$duplicate(from, to, namespace = ns)
    }
  },
  vectorize.args = c("from", "to")
)
