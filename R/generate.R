#' @title Show the analysis wildcard
#' used in \code{\link{plan_summaries}()}.
#' @description Used to generate workflow plan data frames.
#' @export
#' @seealso \code{\link{plan_summaries}()}
#' @return The analysis wildcard used in \code{\link{plan_summaries}()}.
#' @examples
#' # See ?plan_analyses for examples
analysis_wildcard <- function(){
  "analysis__"
}

#' @title Show the dataset wildcard
#' used in \code{\link{plan_analyses}()} and \code{\link{plan_summaries}()}.
#' @description Used to generate workflow plan data frames.
#' @export
#' @seealso \code{\link{plan_analyses}()}
#' @return The dataset wildcard used in
#' \code{\link{plan_analyses}()} and \code{\link{plan_summaries}()}.
#' @examples
#' # See ?plan_analyses for examples
dataset_wildcard <- function(){
  "dataset__"
}

#' @title Use wildcard templating to create a
#' workflow plan data frame from a template data frame.
#' @description The commands in workflow plan data frames can have
#' wildcard symbols that can stand for datasets, parameters, function
#' arguments, etc. These wildcards can be evaluated over a set of
#' possible values using \code{evaluate_plan}.
#' @details Specify a single wildcard with the \code{wildcard}
#' and \code{values} arguments. In each command, the text in
#' \code{wildcard} will be replaced by each value in \code{values}
#' in turn. Specify multiple wildcards with the \code{rules} argument,
#' which overrules \code{wildcard} and \code{values} if
#' not \code{NULL}. Here, \code{rules} should be a list with wildcards
#' as names and vectors of possible values as list elements.
#' @export
#' @return A workflow plan data frame with the wildcards evaluated.
#'
#' @param plan workflow plan data frame, similar to one produced by
#' \code{\link{drake_plan}}
#'
#' @param rules Named list with wildcards as names and vectors of
#' replacements
#' as values. This is a way to evaluate multiple wildcards at once.
#' When not \code{NULL}, \code{rules} overrules \code{wildcard} and
#' \code{values} if
#' not \code{NULL}.
#'
#' @param wildcard character scalar denoting a wildcard placeholder
#'
#' @param values vector of values to replace the wildcard
#' in the drake instructions. Will be treated as a character vector.
#' Must be the same length as \code{plan$command} if \code{expand} is
#' \code{TRUE}.
#'
#' @param expand If \code{TRUE}, create a new rows in the workflow plan
#' data frame
#' if multiple values are assigned to a single wildcard.
#' If \code{FALSE}, each occurrence of the wildcard
#' is replaced with the next entry in the \code{values} vector,
#' and the values are recycled.
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
evaluate_plan <- function(
  plan,
  rules = NULL,
  wildcard = NULL,
  values = NULL,
  expand = TRUE
){
  if (!is.null(rules)){
    return(evaluations(plan = plan, rules = rules, expand = expand))
  }
  if (is.null(wildcard) | is.null(values)){
    return(plan)
  }
  matches <- grepl(wildcard, plan$command, fixed = TRUE)
  if (!any(matches)){
    return(plan)
  }
  major <- unique_random_string(exclude = colnames(plan))
  minor <- unique_random_string(exclude = c(colnames(plan), major))
  plan[[minor]] <- seq_len(nrow(plan))
  plan[[major]] <- plan[[minor]]
  matching <- plan[matches, ]
  if (expand){
    matching <- expand_plan(matching, values)
  }
  values <- rep(values, length.out = nrow(matching))
  matching$command <- Vectorize(
    function(value, command){
      gsub(wildcard, value, command, fixed = TRUE)
    }
    )(values, matching$command)
  rownames(matching) <- NULL
  rownames(plan) <- NULL
  matching[[minor]] <- seq_len(nrow(matching))
  out <- rbind(matching, plan[!matches, ])
  out <- out[order(out[[major]], out[[minor]]), ]
  out[[minor]] <- NULL
  out[[major]] <- NULL
  rownames(out) <- NULL
  return(out)
}

evaluations <- function(
  plan,
  rules = NULL,
  expand = TRUE
  ){
  if (is.null(rules)){
    return(plan)
  }
  stopifnot(is.list(rules))
  for (index in seq_len(length(rules))){
    plan <- evaluate_plan(
      plan,
      wildcard = names(rules)[index],
      values = rules[[index]],
      expand = expand
      )
  }
  return(plan)
}

#' @title Create replicates of targets.
#' @description Duplicates the rows of a workflow plan data frame.
#' Prefixes are appended to the new target names
#' so targets still have unique names.
#' @export
#' @return An expanded workflow plan data frame (with replicated targets).
#' @param plan workflow plan data frame
#' @param values values to expand over. These will be appended to
#' the names of the new targets.
#' @examples
#' # Create the part of the workflow plan for the datasets.
#' datasets <- drake_plan(
#'   small = simulate(5),
#'   large = simulate(50))
#' # Create replicates. If you want repeat targets,
#' # this is convenient.
#' expand_plan(datasets, values = c("rep1", "rep2", "rep3"))
expand_plan <- function(plan, values = NULL){
  if (!length(values)){
    return(plan)
  }
  nrows <- nrow(plan)
  repeat_targets <- rep(seq_len(nrows), each = length(values))
  plan <- plan[repeat_targets, ]
  values <- rep(values, times = nrows)
  plan$target <- paste(plan$target, values, sep = "_")
  rownames(plan) <- NULL
  return(plan)
}

#' @title Write commands to combine several targets into one
#' or more overarching targets.
#' @description Creates a new workflow plan data frame with a single new
#' target. This new target is a list, vector, or other aggregate of
#' a collection of existing targets in another workflow plan data frame.
#' @export
#' @return A workflow plan data frame that aggregates multiple
#' prespecified targets into one additional target downstream.
#' @param plan workflow plan data frame of prespecified targets
#' @param target name of the new aggregated target
#' @param gather function used to gather the targets. Should be
#' one of \code{\link{list}(...)}, \code{\link{c}(...)},
#' \code{\link{rbind}(...)}, or similar.
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
  return(
    data.frame(target = target, command = command, stringsAsFactors = FALSE)
  )
}

#' @title Generate a workflow plan data frame to
#' analyze multiple datasets using multiple methods of analysis.
#' @description Uses wildcards to create a new
#' workflow plan data frame from a template data frame.
#' @seealso \code{\link{plan_summaries}},
#'  \code{\link{make}}, \code{\link{drake_plan}}
#' @export
#' @return An evaluated workflow plan data frame of analysis targets.
#' @param plan workflow plan data frame of analysis methods.
#' The commands in the \code{command} column must
#' have the \code{dataset__} wildcard where the datasets go.
#' For example, one command could be \code{lm(dataset__)}. Then,
#' the commands in the output will include \code{lm(your_dataset_1)},
#' \code{lm(your_dataset_2)}, etc.
#' @param datasets workflow plan data frame with instructions
#' to make the datasets.
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
#' multiple analyses of multiple datasets multiple ways.
#' @description Uses wildcards to create a new
#' workflow plan data frame from a template data frame.
#' @seealso \code{\link{plan_analyses}}, \code{\link{make}},
#' \code{\link{drake_plan}}
#' @export
#' @return An evaluated workflow plan data frame of instructions
#' for computing summaries of analyses and datasets.
#' analyses of multiple datasets in multiple ways.
#' @param plan workflow plan data frame with commands for the summaries.
#' Use the \code{analysis__} and \code{dataset__} wildcards
#' just like the \code{dataset__} wildcard in \code{\link{analyses}()}.
#' @param analyses workflow plan data frame of analysis instructions
#' @param datasets workflow plan data frame with instructions to make
#' or import the datasets.
#' @param gather Character vector, names of functions to gather the
#' summaries. If not \code{NULL}, the length must be the number of
#' rows in the \code{plan}. See the \code{\link{gather}()} function
#' for more.
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
  gathered <- ddply(
    out,
    group,
    function(summary_group){
      summary_type <- summary_group[[group]][1]
      gather_plan(
        summary_group,
        target = summary_type,
        gather = gather[which(summary_type == plan$target)])
    }
    )
  out[[group]] <- NULL
  gathered[[group]] <- NULL
  return(rbind(gathered, out))
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

unique_random_string <- function(n = 30, exclude = NULL){
  if (!length(exclude)){
    return(stri_rand_strings(1, n))
  }
  out <- exclude[1]
  while (out %in% exclude){
    out <- stri_rand_strings(1, n)
    next
  }
  return(out)
}
