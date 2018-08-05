#' @title Show the analysis wildcard
#'   used in [plan_summaries()].
#' @description Used to generate workflow plan data frames.
#' @export
#' @seealso [plan_summaries()]
#' @return The analysis wildcard used in [plan_summaries()].
#' @examples
#' # See ?plan_analyses for examples
analysis_wildcard <- function(){
  "analysis__"
}

#' @title Show the dataset wildcard
#'   used in [plan_analyses()] and [plan_summaries()].
#' @description Used to generate workflow plan data frames.
#' @export
#' @seealso [plan_analyses()]
#' @return The dataset wildcard used in
#'   [plan_analyses()] and [plan_summaries()].
#' @examples
#' # See ?plan_analyses for examples
dataset_wildcard <- function(){
  "dataset__"
}

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
#' @return A workflow plan data frame with the wildcards evaluated.
#'
#' @param plan workflow plan data frame, similar to one produced by
#'   [drake_plan()]
#'
#' @param rules Named list with wildcards as names and vectors of
#'   replacements
#'   as values. This is a way to evaluate multiple wildcards at once.
#'   When not `NULL`, `rules` overrules `wildcard` and
#'   `values` if
#'   not `NULL`.
#'
#' @param wildcard character scalar denoting a wildcard placeholder
#'
#' @param values vector of values to replace the wildcard
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
#' @param rename logical, whether to rename the targets
#'   based on the values supplied for the wildcards
#'   (based on `values` or `rules`).
#'
#' @param trace logical, whether to add columns that
#'   trace the wildcard expansion process. These new
#'   columns indicate which targets were evaluated with which
#'   wildcards.
#'   
#' @param columns character vector of names of columns
#'   to look for and evaluate the wildcards.
#'
#' @examples
#' # Create the part of the workflow plan for the datasets.
#' datasets <- drake_plan(
#'   small = simulate(5),
#'   large = simulate(50))
#' # Create a template workflow plan for the analyses.
#' methods <- drake_plan(
#'   regression1 = reg1(dataset__),
#'   regression2 = reg2(dataset__))
#' # Evaluate the wildcards in the template
#' # to produce the actual part of the workflow plan
#' # that encodes the analyses of the datasets.
#' # Create one analysis for each combination of dataset and method.
#' evaluate_plan(methods, wildcard = "dataset__",
#'   values = datasets$target)
#' # Only choose some combinations of dataset and analysis method.
#' ans <- evaluate_plan(methods, wildcard = "dataset__",
#'   values = datasets$target, expand = FALSE)
#' ans
#' # For the complete workflow plan, row bind the pieces together.
#' my_plan <- rbind(datasets, ans)
#' my_plan
#' # Workflow plans can have multiple wildcards.
#' # Each combination of wildcard values will be used
#' # Except when expand is FALSE.
#' x <- drake_plan(draws = rnorm(mean = Mean, sd = Sd))
#' evaluate_plan(x, rules = list(Mean = 1:3, Sd = c(1, 10)))
#' # You can use wildcards on columns other than "command"
#' evaluate_plan(
#'   drake_plan(
#'     x = target("always", cpu = "any"),
#'     y = target("any", cpu = "always"),
#'     z = target("any", cpu = "any"),
#'     strings_in_dots = "literals"
#'   ),
#'   rules = list(always = 1:2),
#'   columns = c("command", "cpu")
#' )
#' # With the `trace` argument,
#' # you can generate columns that show how the wildcards
#' # were evaluated.
#' plan <- drake_plan(x = rnorm(n__), y = rexp(n__))
#' plan <- evaluate_plan(plan, wildcard = "n__", values = 1:2, trace = TRUE)
#' print(plan)
#' # With the `trace` argument,
#' # you can generate columns that show how the wildcards
#' # were evaluated. Then you can visualize the wildcard groups
#' # as clusters.
#' plan <- drake_plan(x = rnorm(n__), y = rexp(n__))
#' plan <- evaluate_plan(plan, wildcard = "n__", values = 1:2, trace = TRUE)
#' print(plan)
#' cache <- storr::storr_environment()
#' config <- drake_config(plan, cache = cache)
#' vis_drake_graph(config, group = "n__", clusters = "1")
#' vis_drake_graph(config, group = "n__", clusters = c("1", "2"))
#' make(plan, targets = c("x_1", "y_2"), cache = cache)
#' # Optionally cluster on columns supplied by `drake_graph_info()$nodes`.
#' vis_drake_graph(config, group = "status", clusters = "up to date")
evaluate_plan <- function(
  plan,
  rules = NULL,
  wildcard = NULL,
  values = NULL,
  expand = TRUE,
  rename = expand,
  trace = FALSE,
  columns = "command"
){
  if (!is.null(rules)){
    check_wildcard_rules(rules)
    evaluate_wildcard_rules(
      plan = plan,
      rules = rules,
      expand = expand,
      rename = rename,
      trace = trace,
      columns = columns
    )
  } else if (!is.null(wildcard) && !is.null(values)){
    evaluate_single_wildcard(
      plan = plan,
      wildcard = wildcard,
      values = values,
      expand = expand,
      rename = rename,
      trace = trace,
      columns = columns
    )
  } else {
    as_drake_plan(plan)
  }
}

evaluate_single_wildcard <- function(
  plan, wildcard, values, expand, rename, trace, columns
){
  if (!length(columns)){
    return(plan)
  }
  if ("target" %in% columns){
    stop(
      "'target' cannot be in the `columns` argument of evaluate_plan().",
      call = FALSE
    )
  }
  missing_cols <- setdiff(columns, colnames(plan))
  if (length(missing_cols)){
    stop(
      "some columns you selected for evaluate_plan() are not in the plan:\n",
      multiline_message(missing_cols),
      call. = FALSE
    )
  }
  values <- as.character(values)
  matches <- rep(FALSE, nrow(plan))
  for (col in columns){
    matches <- matches | grepl(wildcard, plan[[col]], fixed = TRUE)
  }
  if (!any(matches)){
    return(plan)
  }
  major <- digest::digest(tempfile())
  minor <- digest::digest(tempfile())
  plan[[major]] <- seq_len(nrow(plan))
  plan[[minor]] <- plan[[major]]
  matching <- plan[matches, ]
  if (expand){
    matching <- expand_plan(matching, values, rename = rename)
  } else if (rename){
    matching$target <- paste(matching$target, values, sep = "_")
  }
  values <- rep(values, length.out = nrow(matching))
  for (col in columns){
    matching[[col]] <- Vectorize(
      function(value, command){
        gsub(wildcard, value, command, fixed = TRUE)
      }
    )(values, matching[[col]])
  }
  if (trace){
    matching[[wildcard]] <- values
  }
  rownames(matching) <- NULL
  rownames(plan) <- NULL
  matching[[minor]] <- seq_len(nrow(matching))
  out <- dplyr::bind_rows(matching, plan[!matches, ])
  out <- out[order(out[[major]], out[[minor]]), ]
  out[[minor]] <- NULL
  out[[major]] <- NULL
  rownames(out) <- NULL
  if (trace){
    out <- structure(
      out,
      wildcards = base::union(attr(plan, "wildcards"), wildcard)
    )
  }
  sanitize_plan(out, allow_duplicated_targets = TRUE)
}

evaluate_wildcard_rules <- function(
  plan, rules, expand, rename, trace, columns
){
  for (index in seq_len(length(rules))){
    plan <- evaluate_single_wildcard(
      plan,
      wildcard = names(rules)[index],
      values = rules[[index]],
      expand = expand,
      rename = rename,
      trace = trace,
      columns = columns
    )
  }
  as_drake_plan(plan)
}

check_wildcard_rules <- function(rules){
  stopifnot(is.list(rules))
  wildcards <- names(rules)
  all_values <- unlist(rules)
  for (i in seq_along(wildcards)){
    matches <- grep(wildcards[i], all_values, value = TRUE)
    if (length(matches)){
      stop(
        "No wildcard name can match the name of any replacement value. ",
        "Conflicts: \"", wildcards[i], "\" with:\n",
        multiline_message(paste0("\"", matches, "\"")),
        call. = FALSE
      )
    }
    matches <- grep(wildcards[i], wildcards[-i], value = TRUE)
    if (length(matches)){
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
#' @return An expanded workflow plan data frame (with replicated targets).
#' @param plan workflow plan data frame
#' @param values values to expand over. These will be appended to
#'   the names of the new targets.
#' @param rename logical, whether to rename the targets
#'   based on the `values`. See the examples for a demo.
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
expand_plan <- function(plan, values = NULL, rename = TRUE){
  if (!length(values)){
    return(as_drake_plan(plan))
  }
  nrows <- nrow(plan)
  repeat_targets <- rep(seq_len(nrows), each = length(values))
  plan <- plan[repeat_targets, ]
  values <- as.character(values)
  values <- rep(values, times = nrows)
  if (rename){
    plan$target <- paste(plan$target, values, sep = "_")
  }
  rownames(plan) <- NULL
  sanitize_plan(plan, allow_duplicated_targets = TRUE)
}

#' @title Write commands to combine several targets into one
#'   or more overarching targets.
#' @description Creates a new workflow plan data frame with a single new
#' target. This new target is a list, vector, or other aggregate of
#' a collection of existing targets in another workflow plan data frame.
#' @export
#' @return A workflow plan data frame that aggregates multiple
#'   prespecified targets into one additional target downstream.
#' @param plan workflow plan data frame of prespecified targets
#' @param target name of the new aggregated target
#' @param gather function used to gather the targets. Should be
#'   one of `list(...)`, `c(...)`, `rbind(...)`, or similar.
#' @examples
#' # Workflow plan for datasets:
#' datasets <- drake_plan(
#'   small = simulate(5),
#'   large = simulate(50))
#' # Create a new target that brings the datasets together.
#' gather_plan(datasets, target = "my_datasets")
#' # This time, the new target just appends the rows of 'small' and 'large'
#' # into a single matrix or data frame.
#' gathered <- gather_plan(
#'   datasets, target = "aggregated_data", gather = "rbind"
#' )
#' gathered
#' # For the complete workflow plan, row bind the pieces together.
#' my_plan <- rbind(datasets, gathered)
#' my_plan
gather_plan <- function(
  plan = NULL,
  target = "target",
  gather = "list"
){
  command <- paste(plan$target, "=", plan$target)
  command <- paste(command, collapse = ", ")
  command <- paste0(gather, "(", command, ")")
  tibble(target = target, command = command) %>%
    as_drake_plan()
}

#' @title Write commands to reduce several targets down to one.
#' @description Creates a new workflow plan data frame with the
#'   commands to do a reduction (i.e. to repeatedly apply a binary
#'   operator to pairs of targets to produce one target).
#' @export
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
#' @examples
#' # Workflow plan for datasets:
#' x_plan <- evaluate_plan(
#'   drake_plan(x = VALUE),
#'   wildcard = "VALUE",
#'   values = 1:8
#' )
#' # Create a new target from the sum of the others.
#' reduce_plan(x_plan, target = "x_sum", pairwise = FALSE)
#' # For memory efficiency and parallel computing,
#' # reduce pairwise:
#' reduce_plan(x_plan, target = "x_sum", pairwise = TRUE)
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
  pairwise = TRUE
){
  if (pairwise){
    pairs <- reduction_pairs(
      x = plan$target,
      base_name = paste0(target, "_")
    )
    pairs$names[nrow(pairs)] <- target
    tibble(
      target = pairs$names,
      command = paste0(begin, pairs$odds, op, pairs$evens, end)
    ) %>%
      as_drake_plan()
  } else {
    command <- Reduce(
      x = plan$target,
      f = function(x, y){
        paste0(begin, x, op, y, end)
      }
    )
    tibble(target = target, command = command) %>%
      as_drake_plan()
  }
}

reduction_pairs <- function(x, pairs = NULL, base_name = "reduced_"){
  if (length(x) < 2){
    return(pairs)
  }
  evens <- x[seq(from = 2, to = length(x), by = 2)]
  odds <- x[seq(from = 1, to = length(x), by = 2)]
  names <- new_x <- paste0(base_name, seq_along(odds) + (nrow(pairs) %||% 0))
  if (length(odds) > length(evens)){
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

#' @title Generate a workflow plan data frame to
#'   analyze multiple datasets using multiple methods of analysis.
#' @description Uses wildcards to create a new
#' workflow plan data frame from a template data frame.
#' @seealso [plan_summaries()],
#'    [make()], [drake_plan()]
#' @export
#' @return An evaluated workflow plan data frame of analysis targets.
#' @param plan workflow plan data frame of analysis methods.
#'   The commands in the `command` column must
#'   have the `dataset__` wildcard where the datasets go.
#'   For example, one command could be `lm(dataset__)`. Then,
#'   the commands in the output will include `lm(your_dataset_1)`,
#'   `lm(your_dataset_2)`, etc.
#' @param datasets workflow plan data frame with instructions
#'   to make the datasets.
#' @examples
#' # Create the piece of the workflow plan for the datasets.
#' datasets <- drake_plan(
#'   small = simulate(5),
#'   large = simulate(50))
#' # Create a template for the analysis methods.
#' methods <- drake_plan(
#'   regression1 = reg1(dataset__),
#'   regression2 = reg2(dataset__))
#' # Evaluate the wildcards to create the part of the workflow plan
#' # encoding the analyses of the datasets.
#' ans <- plan_analyses(methods, datasets = datasets)
#' ans
#' # For the final workflow plan, row bind the pieces together.
#' my_plan <- rbind(datasets, ans)
#' my_plan
plan_analyses <- function(plan, datasets){
  plan <- deprecate_wildcard(
    plan = plan,
    old = "..dataset..",
    replacement = dataset_wildcard()
  )
  evaluate_plan(
    plan,
    wildcard = dataset_wildcard(),
    values = datasets$target
  )
}

#' @title Generate a workflow plan data frame for summarizing
#'   multiple analyses of multiple datasets multiple ways.
#' @description Uses wildcards to create a new
#' workflow plan data frame from a template data frame.
#' @seealso [plan_analyses()], [make()],
#'   [drake_plan()]
#' @export
#' @return An evaluated workflow plan data frame of instructions
#'   for computing summaries of analyses and datasets.
#'   analyses of multiple datasets in multiple ways.
#' @param plan workflow plan data frame with commands for the summaries.
#'   Use the `analysis__` and `dataset__` wildcards
#'   just like the `dataset__` wildcard in [plan_analyses()].
#' @param analyses workflow plan data frame of analysis instructions
#' @param datasets workflow plan data frame with instructions to make
#'   or import the datasets.
#' @param gather Character vector, names of functions to gather the
#'   summaries. If not `NULL`, the length must be the number of
#'   rows in the `plan`. See the [gather_plan()] function
#'   for more.
#' @examples
#' # Create the part of the workflow plan data frame for the datasets.
#' datasets <- drake_plan(
#'   small = simulate(5),
#'   large = simulate(50))
#' # Create a template workflow plan containing the analysis methods.
#' methods <- drake_plan(
#'   regression1 = reg1(dataset__),
#'   regression2 = reg2(dataset__))
#' # Generate the part of the workflow plan to analyze the datasets.
#' analyses <- plan_analyses(methods, datasets = datasets)
#' # Create a template workflow plan dataset with the
#' # types of summaries you want.
#' summary_types <- drake_plan(
#'   summ = summary(analysis__),
#'   coef = coefficients(analysis__))
#' # Evaluate the appropriate wildcards to encode the summary targets.
#' plan_summaries(summary_types, analyses, datasets, gather = NULL)
#' plan_summaries(summary_types, analyses, datasets)
#' plan_summaries(summary_types, analyses, datasets, gather = "list")
#' summs <- plan_summaries(
#'   summary_types, analyses, datasets, gather = c("list", "rbind"))
#' # For the final workflow plan, row bind the pieces together.
#' my_plan <- rbind(datasets, analyses, summs)
#' my_plan
plan_summaries <- function(
  plan,
  analyses,
  datasets,
  gather = rep("list", nrow(plan))
){
  plan <- deprecate_wildcard(
    plan = plan,
    old = "..analysis..",
    replacement = analysis_wildcard()
  )
  plan <- deprecate_wildcard(
    plan = plan,
    old = "..dataset..",
    replacement = dataset_wildcard()
  )
  plan <- with_analyses_only(plan)
  out <- plan
  group <- paste(colnames(out), collapse = "_")
  out[[group]] <- out$target
  if (!any(grepl(analysis_wildcard(), out$command, fixed = TRUE))){
    stop(
      "no 'analysis__' wildcard found in plan$command. ",
      "Use plan_analyses() instead."
      )
  }
  out <- evaluate_plan(
    out,
    wildcard = analysis_wildcard(),
    values = analyses$target
  )
  out <- evaluate_plan(
    out,
    wildcard = dataset_wildcard(),
    values = datasets$target,
    expand = FALSE
  )
  if (!length(gather)){
    return(out[setdiff(names(out), group)])
  }
  if (length(gather) == 1){
    gather <- rep(gather, dim(plan)[1])
  }
  if (!(length(gather) == dim(plan)[1])){
    stop("gather must be NULL or have length 1 or nrow(plan)")
  }
  . <- group_sym <- as.symbol(group)
  gathered <- group_by(out, !!!group_sym) %>%
    do({
      summary_type <- .[[group]][1]
      gather_plan(
        .,
        target = summary_type,
        gather = gather[which(summary_type == plan$target)]
      )
    })
  target <- command <- NULL
  dplyr::bind_rows(gathered, out) %>%
    ungroup %>%
    select(target, command) %>%
    as_drake_plan()
}

with_analyses_only <- function(plan){
  has_analysis <- grepl(analysis_wildcard(), plan$command, fixed = TRUE)
  if (any(!has_analysis)){
    warning(
      "removing ",
      sum(has_analysis),
      " rows with no 'analysis__' wildcard in the command.",
      "Use plan_analyses() for these.",
      call. = FALSE
    )
  }
  return(plan[has_analysis, ])
}
