#' @title Create a plan that maps a function to a grid of arguments.
#' @description `map_plan()` is like `base::Map()`:
#'   it takes a function name and a grid of arguments, and 
#'   writes out all the commands calls to apply the function to
#'   each row of arguments.
#' @export
#' @seealso [drake_plan()], [reduce_by()], [gather_by()], [reduce_plan()], [gather_plan()],
#'   [evaluate_plan()], [expand_plan()]
#' @return A workflow plan data frame.
#' @param args a data frame (or better yet, a `tibble`)
#'   of function arguments to `fun`.
#'   Here, the column names should be the names of the arguments
#'   of `fun`, and each row of `args` corresponds to a
#'   call to `fun`.
#' @param fun name of a function to apply the arguments
#'   row-by-row. Supply a symbol if `character_only` is
#'   `FALSE` and a character scalar otherwise.
#' @param id name of an optional column in `args`
#'   giving the names of the targets. If not supplied,
#'   target names will be generated automatically.
#'   `id` should be a symbol if `character_only` is `FALSE`
#'   and a character scalar otherwise.
#' @param character_only logical, whether to interpret
#'   the `fun` and `id` arguments as character scalars or symbols.
#' @param trace logical, whether to append the columns of `args`
#'   to the output workflow plan data frame. The added columns
#'   help "trace back" the original settings that went into building
#'   each target. Similar to the `trace` argument of [evaluate_plan()].
#' @examples
#' # For the full tutorial, visit
#' # https://ropenscilabs.github.io/drake-manual/plans.html#map_plan.
#' my_model_fit <- function(x1, x2, data) {
#'   lm(as.formula(paste("mpg ~", x1, "+", x1)), data = data)
#' }
#' covariates <- setdiff(colnames(mtcars), "mpg")
#' args <- t(combn(covariates, 2))
#' colnames(args) <- c("x1", "x2")
#' args <- tibble::as_tibble(args)
#' args$data <- "mtcars"
#' args$data <- rlang::syms(args$data)
#' args$id <- paste0("fit_", args$x1, "_", args$x2)
#' args
#' plan <- map_plan(args, my_model_fit)
#' plan
#' # Consider `trace = TRUE` to include the columns in `args`
#' # in your plan.
#' map_plan(args, my_model_fit, trace = TRUE)
#' # And of course, you can execute the plan and
#' # inspect your results.
#' cache <- storr::storr_environment()
#' make(plan, verbose = FALSE, cache = cache)
#' readd(fit_cyl_disp, cache = cache)
map_plan <- function(
  args,
  fun,
  id = "id",
  character_only = FALSE,
  trace = FALSE
) {
  args <- tibble::as_tibble(args)
  if (!character_only) {
    fun <- as.character(substitute(fun))
    id <- as.character(substitute(id))
  }
  cols <- setdiff(colnames(args), id)
  if (id %in% colnames(args)) {
    target <- args[[id]]
  } else {
    target <- paste0(
      fun, "_",
      apply(X = args, MARGIN = 1, FUN = digest::digest, algo = "murmur32")
    )
  }
  command <- as.character(unlist(drake_pmap(
    .l = args[, cols],
    .f = function(...) {
      out <- list(as.name(fun), ...)
      out <- as.call(out)
      rlang::expr_text(out)
    }
  )))
  out <- tibble::tibble(target = target, command = command)
  if (trace) {
    out <- tibble::as_tibble(cbind(out, args))
  }
  sanitize_plan(out)
}

#' @title Write commands to combine several targets into one
#'   or more overarching targets.
#' @description Creates a new workflow plan to aggregate
#'   existing targets in the supplied plan.
#' @export
#' @seealso [drake_plan(), [map_plan()], [reduce_by()], [gather_by()], [reduce_plan()],
#'   [evaluate_plan()], [expand_plan()]
#' @return A workflow plan data frame that aggregates multiple
#'   prespecified targets into one additional target downstream.
#' @param plan workflow plan data frame of prespecified targets
#' @param target name of the new aggregated target
#' @param gather function used to gather the targets. Should be
#'   one of `list(...)`, `c(...)`, `rbind(...)`, or similar.
#' @param append logical. If `TRUE`, the output will include the
#'   original rows in the `plan` argument.
#'   If `FALSE`, the output will only include the new
#'   targets and commands.
#' @examples
#' # Workflow plan for datasets:
#' datasets <- drake_plan(
#'   small = simulate(5),
#'   large = simulate(50)
#' )
#' # Create a new target that brings the datasets together.
#' gather_plan(datasets, target = "my_datasets", append = FALSE)
#' # This time, the new target just appends the rows of 'small' and 'large'
#' # into a single matrix or data frame.
#' gathered <- gather_plan(
#'   datasets,
#'   target = "aggregated_data",
#'   gather = "rbind",
#'   append = FALSE
#' )
#' gathered
#' # For the complete workflow plan, row bind the pieces together.
#' bind_plans(datasets, gathered)
#' # Alternatively, you can set `append = TRUE` to incorporate
#' # the new targets automatically.
#' gather_plan(
#'   datasets,
#'   target = "aggregated_data",
#'   gather = "rbind",
#'   append = TRUE
#' )
gather_plan <- function(
  plan = NULL,
  target = "target",
  gather = "list",
  append = FALSE
) {
  command <- paste(plan$target, "=", plan$target)
  command <- paste(command, collapse = ", ")
  command <- paste0(gather, "(", command, ")")
  new_plan <- tibble(target = target, command = command)
  if (append) {
    bind_plans(plan, new_plan)
  } else {
    new_plan
  }
}

#' @title Gather multiple groupings of targets
#' @description Perform several calls to [gather_plan()]
#'   based on groupings from columns in the plan,
#'   and then row-bind the new targets to the plan.
#' @export
#' @seealso [drake_plan()], [map_plan()], [reduce_by()], [reduce_plan()],
#'   [gather_plan()], [evaluate_plan()], [expand_plan()]
#' @return A workflow plan data frame.
#' @inheritParams gather_plan
#' @param ... Symbols, columns of `plan` to define target groupings.
#'   A [gather_plan()] call is applied for each grouping.
#'   Groupings with all `NA`s in the selector variables are ignored.
#' @param prefix character, prefix for naming the new targets.
#'   Suffixes are generated from the values of the columns
#'   specified in `...`.
#' @param filter an expression like you would pass to `dplyr::filter()`.
#'   The rows for which `filter` evaluates to `TRUE` will be gathered,
#'   and the rest will be excluded from gathering.
#'   Why not just call `dplyr::filter()` before `gather_by()`?
#'   Because `gather_by(append = TRUE, filter = my_column == "my_value")`
#'   gathers on some targets while including all the original targets
#'   in the output. See the examples for a demonstration.
#' @param sep character scalar, delimiter for creating the names
#'   of new targets
#' @examples
#' plan <- drake_plan(
#'   data = get_data(),
#'   informal_look = inspect_data(data, mu = mu__),
#'   bayes_model = bayesian_model_fit(data, prior_mu = mu__)
#' )
#' plan <- evaluate_plan(plan, rules = list(mu__ = 1:2), trace = TRUE)
#' plan
#' gather_by(plan, mu___from, gather = "rbind")
#' gather_by(plan, mu___from, append = TRUE)
#' gather_by(plan, mu___from, append = FALSE)
#' gather_by(plan, mu__, mu___from, prefix = "x")
#' gather_by(plan) # Gather everything and append a row.
#' # You can filter out the informal_look_* targets beforehand
#' # if you only want the bayes_model_* ones to be gathered.
#' # The advantage here is that if you also need `append = TRUE`,
#' # only the bayes_model_* targets will be gathered, but
#' # the informal_look_* targets will still be included
#' # in the output.
#' gather_by(
#'   plan,
#'   mu___from,
#'   append = TRUE,
#'   filter = mu___from == "bayes_model"
#' )
gather_by <- function(
  plan,
  ...,
  prefix = "target",
  gather = "list",
  append = TRUE,
  filter = NULL,
  sep = "_"
) {
  gathered <- plan
  if (!is.null(substitute(filter))) {
    filter <- rlang::enquo(filter)
    selection <- rlang::eval_tidy(expr = filter, data = gathered)
    selection[is.na(selection)] <- FALSE
    gathered <- gathered[selection, ]
  }
  col_names <- as.character(match.call(expand.dots = FALSE)$...)
  gathered <- map_by(
    .x = gathered,
    .by = col_names,
    .f = gather_plan,
    target = prefix,
    gather = gather,
    append = FALSE
  )
  cols <- gathered[, col_names]
  suffix <- apply(X = cols, MARGIN = 1, FUN = paste, collapse = sep)
  if (length(suffix) && nzchar(suffix)) {
    gathered$target <- paste(gathered$target, suffix, sep = sep)
  }
  if (append) {
    out <- bind_plans(plan, gathered)
  } else {
    out <- gathered
  }
  arrange_plan_cols(out)
}

#' @title Write commands to reduce several targets down to one.
#' @description Creates a new workflow plan data frame with the
#'   commands to do a reduction (i.e. to repeatedly apply a binary
#'   operator to pairs of targets to produce one target).
#' @export
#' @seealso [drake_plan()], [map_plan()], [reduce_by()], [gather_by()],
#'   [gather_plan()], [evaluate_plan()], [expand_plan()]
#' @return A workflow plan data frame that aggregates multiple
#'   prespecified targets into one additional target downstream.
#' @param plan workflow plan data frame of prespecified targets
#' @param target name of the new reduced target
#' @param begin character, code to place at the beginning
#'   of each step in the reduction
#' @param op binary operator to apply in the reduction
#' @param end character, code to place at the end
#'   of each step in the reduction
#' @param pairwise logical, whether to create multiple
#'   new targets, one for each pair/step in the reduction (`TRUE`),
#'   or to do the reduction all in one command.
#' @param append logical. If `TRUE`, the output will include the
#'   original rows in the `plan` argument.
#'   If `FALSE`, the output will only include the new
#'   targets and commands.
#' @param sep character scalar, delimiter for creating new target names
#' @examples
#' # Workflow plan for datasets:
#' x_plan <- evaluate_plan(
#'   drake_plan(x = VALUE),
#'   wildcard = "VALUE",
#'   values = 1:8
#' )
#' x_plan
#' # Create a new target from the sum of the others.
#' reduce_plan(x_plan, target = "x_sum", pairwise = FALSE, append = FALSE)
#' # Optionally include the original rows with `append = TRUE`.
#' reduce_plan(x_plan, target = "x_sum", pairwise = FALSE, append = TRUE)
#' # For memory efficiency and parallel computing,
#' # reduce pairwise:
#' reduce_plan(x_plan, target = "x_sum", pairwise = TRUE, append = FALSE)
#' # Optionally define your own function and use it as the
#' # binary operator in the reduction.
#' x_plan <- evaluate_plan(
#'   drake_plan(x = VALUE),
#'   wildcard = "VALUE",
#'   values = 1:9
#' )
#' x_plan
#' reduce_plan(
#'   x_plan, target = "x_sum", pairwise = TRUE,
#'   begin = "fun(", op = ", ", end = ")"
#' )
reduce_plan <- function(
  plan = NULL,
  target = "target",
  begin = "",
  op = " + ",
  end = "",
  pairwise = TRUE,
  append = FALSE,
  sep = "_"
) {
  if (pairwise) {
    pairs <- reduction_pairs(
      x = plan$target,
      base_name = paste0(target, sep)
    )
    pairs$names[nrow(pairs)] <- target
    out <- tibble(
      target = pairs$names,
      command = paste0(begin, pairs$odds, op, pairs$evens, end)
    )
  } else {
    command <- Reduce(
      x = plan$target,
      f = function(x, y) {
        paste0(begin, x, op, y, end)
      }
    )
    out <- tibble(target = target, command = command)
  }
  if (append) {
    bind_plans(plan, out)
  } else{
    out
  }
}

#' @title Reduce multiple groupings of targets
#' @description Perform several calls to [reduce_plan()]
#'   based on groupings from columns in the plan,
#'   and then row-bind the new targets to the plan.
#' @export
#' @seealso [drake_plan()], [map_plan()], [gather_by()], [reduce_plan()],
#'   [gather_plan()], [evaluate_plan()], [expand_plan()]
#' @return A workflow plan data frame.
#' @inheritParams reduce_plan
#' @param ... Symbols, columns of `plan` to define target groupings.
#'   A [reduce_plan()] call is applied for each grouping.
#'   Groupings with all `NA`s in the selector variables are ignored.
#' @param prefix character, prefix for naming the new targets.
#'   Suffixes are generated from the values of the columns
#'   specified in `...`.
#' @param filter an expression like you would pass to `dplyr::filter()`.
#'   The rows for which `filter` evaluates to `TRUE` will be gathered,
#'   and the rest will be excluded from gathering.
#'   Why not just call `dplyr::filter()` before `gather_by()`?
#'   Because `gather_by(append = TRUE, filter = my_column == "my_value")`
#'   gathers on some targets while including all the original targets
#'   in the output. See the examples for a demonstration.
#' @param sep character scalar, delimiter for creating the names
#'   of new targets
#' @examples
#' plan <- drake_plan(
#'   data = get_data(),
#'   informal_look = inspect_data(data, mu = mu__),
#'   bayes_model = bayesian_model_fit(data, prior_mu = mu__)
#' )
#' plan <- evaluate_plan(plan, rules = list(mu__ = 1:2), trace = TRUE)
#' plan
#' reduce_by(plan, mu___from, begin = "list(", end = ")", op = ", ")
#' reduce_by(plan, mu__)
#' reduce_by(plan, mu__, append = TRUE)
#' reduce_by(plan, mu__, append = FALSE)
#' reduce_by(plan) # Reduce all the targets.
#' reduce_by(plan, append = FALSE)
#' reduce_by(plan, pairwise = FALSE)
#' # You can filter out the informal_look_* targets beforehand
#' # if you only want the bayes_model_* ones to be reduced.
#' # The advantage here is that if you also need `append = TRUE`,
#' # only the bayes_model_* targets will be reduced, but
#' # the informal_look_* targets will still be included
#' # in the output.
#' reduce_by(
#'   plan,
#'   mu___from,
#'   append = TRUE,
#'   filter = mu___from == "bayes_model"
#' )
reduce_by <- function(
  plan,
  ...,
  prefix = "target",
  begin = "",
  op = " + ",
  end = "",
  pairwise = TRUE,
  append = TRUE,
  filter = NULL,
  sep = "_"
) {
  reduced <- plan
  if (!is.null(substitute(filter))) {
    filter <- rlang::enquo(filter)
    selection <- rlang::eval_tidy(expr = filter, data = reduced)
    selection[is.na(selection)] <- FALSE
    reduced <- reduced[selection, ]
  }
  col_names <- as.character(match.call(expand.dots = FALSE)$...)
  reduced <- map_by(
    .x = reduced,
    .by = col_names,
    .f = reduce_plan,
    target = prefix,
    begin = begin,
    op = op,
    end = end,
    pairwise = pairwise,
    append = FALSE,
    sep = sep
  )
  cols <- reduced[, col_names]
  suffix <- apply(X = cols, MARGIN = 1, FUN = paste, collapse = sep)
  if (length(suffix) && nzchar(suffix)) {
    reduced$target <- paste(reduced$target, suffix, sep = sep)
  }
  if (append) {
    out <- bind_plans(plan, reduced)
  } else {
    out <- reduced
  }
  arrange_plan_cols(out)
}

reduction_pairs <- function(x, pairs = NULL, base_name = "reduced_") {
  if (length(x) < 2) {
    return(pairs)
  }
  evens <- x[seq(from = 2, to = length(x), by = 2)]
  odds <- x[seq(from = 1, to = length(x), by = 2)]
  names <- new_x <- paste0(base_name, seq_along(odds) + (nrow(pairs) %||% 0))
  if (length(odds) > length(evens)) {
    evens[length(evens) + 1] <- names[1]
    new_x <- new_x[-1]
  }
  new_pairs <- data.frame(
    names = names, odds = odds, evens = evens,
    stringsAsFactors = FALSE
  )
  reduction_pairs(
    x = new_x,
    pairs = rbind(pairs, new_pairs),
    base_name = base_name
  )
}
