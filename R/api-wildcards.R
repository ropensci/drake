#' @title Use wildcard templating to create a
#'   workflow plan data frame from a template data frame.
#' @description The commands in workflow plan data frames can have
#' wildcard symbols that can stand for datasets, parameters, function
#' arguments, etc. These wildcards can be evaluated over a set of
#' possible values using `evaluate_plan`.
#' @details Specify a single wildcard with the `wildcard`
#' and `values` arguments. In each command, the text in
#' `wildcard` will be replaced by each value in `values`
#' in turn. Specify multiple wildcards with the `rules` argument,
#' which overrules `wildcard` and `values` if
#' not `NULL`. Here, `rules` should be a list with wildcards
#' as names and vectors of possible values as list elements.
#' @export
#' @seealso [drake_plan()], [map_plan()], [reduce_by()], [gather_by()], [reduce_plan()],
#'   [gather_plan()], [evaluate_plan()], [expand_plan()]
#' @return A workflow plan data frame with the wildcards evaluated.
#'
#' @param plan Workflow plan data frame, similar to one produced by
#'   [drake_plan()].
#'
#' @param rules Named list with wildcards as names and vectors of
#'   replacements
#'   as values. This is a way to evaluate multiple wildcards at once.
#'   When not `NULL`, `rules` overrules `wildcard` and
#'   `values` if
#'   not `NULL`.
#'
#' @param wildcard Character scalar denoting a wildcard placeholder.
#'
#' @param values Vector of values to replace the wildcard
#'   in the drake instructions. Will be treated as a character vector.
#'   Must be the same length as `plan$command` if `expand` is
#'   `TRUE`.
#'
#' @param expand If `TRUE`, create a new rows in the workflow plan
#'   data frame
#'   if multiple values are assigned to a single wildcard.
#'   If `FALSE`, each occurrence of the wildcard
#'   is replaced with the next entry in the `values` vector,
#'   and the values are recycled.
#'
#' @param rename Logical, whether to rename the targets
#'   based on the values supplied for the wildcards
#'   (based on `values` or `rules`).
#'
#' @param trace Logical, whether to add columns that
#'   trace the wildcard expansion process. These new
#'   columns indicate which targets were evaluated and with which
#'   wildcards.
#'   
#' @param columns Character vector of names of columns
#'   to look for and evaluate the wildcards.
#'
#' @param sep Character scalar, separator for the names
#'   of the new targets generated. For example, in
#'   `evaluate_plan(drake_plan(x = sqrt(y__)), list(y__ = 1:2), sep = ".")`,
#'   the names of the new targets are `x.1` and `x.2`.
#'
#' @examples
#' # Create the part of the workflow plan for the datasets.
#' datasets <- drake_plan(
#'   small = simulate(5),
#'   large = simulate(50)
#' )
#' # Create a template workflow plan for the analyses.
#' methods <- drake_plan(
#'   regression1 = reg1(dataset__),
#'   regression2 = reg2(dataset__)
#' )
#' # Evaluate the wildcards in the template
#' # to produce the actual part of the workflow plan
#' # that encodes the analyses of the datasets.
#' # Create one analysis for each combination of dataset and method.
#' evaluate_plan(
#'   methods,
#'   wildcard = "dataset__",
#'   values = datasets$target
#' )
#' # Only choose some combinations of dataset and analysis method.
#' ans <- evaluate_plan(
#'   methods,
#'   wildcard = "dataset__",
#'   values = datasets$target,
#'   expand = FALSE
#' )
#' ans
#' # For the complete workflow plan, row bind the pieces together.
#' my_plan <- rbind(datasets, ans)
#' my_plan
#' # Wildcards for evaluate_plan() do not need the double-underscore suffix.
#' # Any valid symbol will do.
#' plan <- drake_plan(
#'   numbers = sample.int(n = `{Number}`, size = ..size)
#' )
#' evaluate_plan(
#'   plan,
#'   rules = list(
#'     "`{Number}`" = c(10, 13),
#'     ..size = c(3, 4)
#'   )
#' )
#' # Workflow plans can have multiple wildcards.
#' # Each combination of wildcard values will be used
#' # Except when expand is FALSE.
#' x <- drake_plan(draws = sample.int(n = N, size = Size))
#' evaluate_plan(x, rules = list(N = 10:13, Size = 1:2))
#' # You can use wildcards on columns other than "command"
#' evaluate_plan(
#'   drake_plan(
#'     x = target("1 + 1", cpu = "any"),
#'     y = target("sqrt(4)", cpu = "always"),
#'     z = target("sqrt(16)", cpu = "any")
#'   ),
#'   rules = list(always = 1:2),
#'   columns = c("command", "cpu")
#' )
#' # With the `trace` argument,
#' # you can generate columns that show how the wildcards
#' # were evaluated.
#' plan <- drake_plan(x = sample.int(n__), y = sqrt(n__))
#' plan <- evaluate_plan(plan, wildcard = "n__", values = 1:2, trace = TRUE)
#' print(plan)
#' # With the `trace` argument,
#' # you can generate columns that show how the wildcards
#' # were evaluated. Then you can visualize the wildcard groups
#' # as clusters.
#' plan <- drake_plan(x = sqrt(n__), y = sample.int(n__))
#' plan <- evaluate_plan(plan, wildcard = "n__", values = 1:2, trace = TRUE)
#' print(plan)
#' cache <- storr::storr_environment()
#' config <- drake_config(plan, cache = cache)
#' \dontrun{
#' if (requireNamespace("visNetwork", quietly = TRUE)) {
#' vis_drake_graph(config, group = "n__", clusters = "1")
#' vis_drake_graph(config, group = "n__", clusters = c("1", "2"))
#' make(plan, targets = c("x_1", "y_2"), cache = cache)
#' # Optionally cluster on columns supplied by `drake_graph_info()$nodes`.
#' vis_drake_graph(config, group = "status", clusters = "up to date")
#' }
#' }
evaluate_plan <- function(
  plan,
  rules = NULL,
  wildcard = NULL,
  values = NULL,
  expand = TRUE,
  rename = expand,
  trace = FALSE,
  columns = "command",
  sep = "_"
) {
  if (!is.null(rules)) {
    check_wildcard_rules(rules)
    evaluate_wildcard_rules(
      plan = plan,
      rules = rules,
      expand = expand,
      rename = rename,
      trace = trace,
      columns = columns,
      sep = sep
    )
  } else if (!is.null(wildcard) && !is.null(values)) {
    evaluate_single_wildcard(
      plan = plan,
      wildcard = wildcard,
      values = values,
      expand = expand,
      rename = rename,
      trace = trace,
      columns = columns,
      sep = sep
    )
  } else {
    plan
  }
}

evaluate_single_wildcard <- function(
  plan,
  wildcard,
  values,
  expand,
  rename,
  trace,
  columns,
  sep
) {
  if (!length(columns)) {
    return(plan)
  }
  if ("target" %in% columns) {
    stop(
      "'target' cannot be in the `columns` argument of evaluate_plan().",
      call = FALSE
    )
  }
  missing_cols <- setdiff(columns, colnames(plan))
  if (length(missing_cols)) {
    stop(
      "some columns you selected for evaluate_plan() are not in the plan:\n",
      multiline_message(missing_cols),
      call. = FALSE
    )
  }
  values <- as.character(values)
  matches <- rep(FALSE, nrow(plan))
  for (col in columns) {
    matches <- matches | grepl(wildcard, plan[[col]], fixed = TRUE)
  }
  if (!any(matches)) {
    return(plan)
  }
  major <- make.names(tempfile())
  minor <- make.names(tempfile())
  plan[[major]] <- seq_len(nrow(plan))
  plan[[minor]] <- plan[[major]]
  matching <- plan[matches, ]
  if (expand) {
    matching <- expand_plan(matching, values, rename = FALSE)
  }
  matched_targets <- matching$target
  if (rename) {
    matching$target <- paste(matching$target, values, sep = sep)
  }
  values <- rep(values, length.out = nrow(matching))
  for (col in columns) {
    matching[[col]] <- Vectorize(
      function(value, command) {
        gsub(wildcard, value, command, fixed = TRUE)
      }
    )(values, matching[[col]])
  }
  if (trace) {
    matching[[wildcard]] <- values
    matching[[paste0(wildcard, "_from")]] <- matched_targets
  }
  rownames(matching) <- NULL
  rownames(plan) <- NULL
  matching[[minor]] <- seq_len(nrow(matching))
  out <- bind_plans(matching, plan[!matches, ])
  out <- out[order(out[[major]], out[[minor]]), ]
  out[[minor]] <- NULL
  out[[major]] <- NULL
  rownames(out) <- NULL
  sanitize_plan(out, allow_duplicated_targets = TRUE)
}

evaluate_wildcard_rules <- function(
  plan, rules, expand, rename, trace, columns, sep
) {
  for (index in seq_len(length(rules))) {
    plan <- evaluate_single_wildcard(
      plan,
      wildcard = names(rules)[index],
      values = rules[[index]],
      expand = expand,
      rename = rename,
      trace = trace,
      columns = columns,
      sep = sep
    )
  }
  plan
}

check_wildcard_rules <- function(rules) {
  stopifnot(is.list(rules))
  wildcards <- names(rules)
  all_values <- unlist(rules)
  for (i in seq_along(wildcards)) {
    matches <- grep(wildcards[i], all_values, fixed = TRUE, value = TRUE)
    if (length(matches)) {
      stop(
        "No wildcard name can match the name of any replacement value. ",
        "Conflicts: \"", wildcards[i], "\" with:\n",
        multiline_message(paste0("\"", matches, "\"")),
        call. = FALSE
      )
    }
    matches <- grep(wildcards[i], wildcards[-i], fixed = TRUE, value = TRUE)
    if (length(matches)) {
      stop(
        "The name of a wildcard cannot be a substring ",
        "of any other wildcard name. ",
        "Conflicts: \"", wildcards[i], "\" with:\n",
        multiline_message(paste0("\"", matches, "\"")),
        call. = FALSE
      )
    }
  }
}

#' @title Create replicates of targets.
#' @description Duplicates the rows of a workflow plan data frame.
#' Prefixes are appended to the new target names
#' so targets still have unique names.
#' @export
#' @seealso [drake_plan()], [map_plan()], [reduce_by()], [gather_by()], [reduce_plan()],
#'   [gather_plan()], [evaluate_plan()], [expand_plan()]
#' @return An expanded workflow plan data frame (with replicated targets).
#' @param plan Workflow plan data frame.
#' @param values Values to expand over. These will be appended to
#'   the names of the new targets.
#' @param rename Logical, whether to rename the targets
#'   based on the `values`. See the examples for a demo.
#' @param sep Character scalar, delimiter between the original
#'   target names and the values to append to create the new
#'   target names. Only relevant when `rename` is `TRUE`.
#' @examples
#' # Create the part of the workflow plan for the datasets.
#' datasets <- drake_plan(
#'   small = simulate(5),
#'   large = simulate(50))
#' # Create replicates. If you want repeat targets,
#' # this is convenient.
#' expand_plan(datasets, values = c("rep1", "rep2", "rep3"))
#' # Choose whether to rename the targets based on the values.
#' expand_plan(datasets, values = 1:3, rename = TRUE)
#' expand_plan(datasets, values = 1:3, rename = FALSE)
expand_plan <- function(plan, values = NULL, rename = TRUE, sep = "_") {
  if (!length(values)) {
    return(plan)
  }
  nrows <- nrow(plan)
  repeat_targets <- rep(seq_len(nrows), each = length(values))
  plan <- plan[repeat_targets, ]
  values <- as.character(values)
  values <- rep(values, times = nrows)
  if (rename) {
    plan$target <- paste(plan$target, values, sep = sep)
  }
  rownames(plan) <- NULL
  sanitize_plan(plan, allow_duplicated_targets = TRUE)
}
