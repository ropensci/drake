#' @title Function \code{evaluate}
#' @description The commands in workflow plan data frames can have
#' wildcard symbols that can stand for datasets, parameters, function
#' arguments, etc. These wildcards can be evaluated over a set of
#' possible values using \code{evaluate}.
#' @details Specify a single wildcard with the \code{wildcard}
#' and \code{values} arguments. In each command, the text in
#' \code{wildcard} will be replaced by each value in \code{values}
#' in turn. Specify multiple wildcards with the \code{rules} argument,
#' which overrules \code{wildcard} and \code{values} if
#' not \code{NULL}. Here, \code{rules} should be a list with wildcards
#' as names and vectors of possible values as list elements.
#' @export
#' @return a workflow plan data frame with the wildcards evaluated
#' @param plan workflow plan data frame, similar to one produced by
#' \code{link{plan}}
#' @param rules Named list with wildcards as names and vectors of
#' replacements
#' as values. This is a way to evaluate multiple wildcards at once.
#' When not \code{NULL}, \code{rules} overrules \code{wildcard} and
#' \code{values} if
#' not \code{NULL}.
#' @param wildcard character scalar denoting a wildcard placeholder
#' @param values vector of values to replace the wildcard
#' in the drake instructions. Will be treated as a character vector.
#' Must be the same length as \code{plan$command} if \code{expand} is
#' \code{TRUE}.
#' @param expand If \code{TRUE}, create a new rows in the workflow plan
#' data frame
#' if multiple values are assigned to a single wildcard.
#' If \code{FALSE}, each occurence of the wildcard
#' is replaced with the next entry in the \code{values} vector,
#' and the values are recycled.
#' @examples
#' datasets <- plan(
#'   small = simulate(5),
#'   large = simulate(50))
#' methods <- plan(
#'   regression1 = reg1(..dataset..),
#'   regression2 = reg2(..dataset..))
#' evaluate(methods, wildcard = "..dataset..",
#'   values = datasets$target)
#' evaluate(methods, wildcard = "..dataset..",
#'   values = datasets$target, expand = FALSE)
#' x = plan(draws = rnorm(mean = Mean, sd = Sd))
#' evaluate(x, rules = list(Mean = 1:3, Sd = c(1, 10)))
evaluate <- function(
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
  major <- unique_random_string(colnames(plan))
  minor <- unique_random_string(c(colnames(plan), major))
  plan[[minor]] <- seq_len(nrow(plan))
  plan[[major]] <- plan[[minor]]
  matching <- plan[matches, ]
  if (expand){
    matching <- expand(matching, values)
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
    plan <- evaluate(
      plan,
      wildcard = names(rules)[index],
      values = rules[[index]],
      expand = expand
      )
  }
  return(plan)
}

#' @title Function \code{expand}
#' @description Expands a workflow plan data frame by duplicating rows.
#' This generates multiple replicates of targets with the same commands.
#' @export
#' @return an expanded workflow plan data frame
#' @param plan workflow plan data frame
#' @param values values to expand over. These will be appended to
#' the names of the new targets.
#' @examples
#' datasets <- plan(
#'   small = simulate(5),
#'   large = simulate(50))
#' expand(datasets, values = c("rep1", "rep2", "rep3"))
expand <- function(plan, values = NULL){
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

#' @title Function \code{gather}
#' @description Create a new workflow plan data frame with a single new
#' target. This new target is a list, vector, or other aggregate of
#' a collection of existing targets in another workflow plan data frame.
#' @export
#' @return workflow plan data frame for aggregating prespecified targets
#' @param plan workflow plan data frame of prespecified targets
#' @param target name of the new aggregated target
#' @param gather function used to gather the targets. Should be
#' one of \code{\link{list}(...)}, \code{\link{c}(...)},
#' \code{\link{rbind}(...)}, or similar.
#' @examples
#' datasets <- plan(
#'   small = simulate(5),
#'   large = simulate(50))
#' gather(datasets, target = "my_datasets")
#' gather(datasets, target = "aggregated_data", gather = "rbind")
gather <- function(
  plan = NULL,
  target = "target",
  gather = "list"
  ){
  command <- paste(plan$target, "=", plan$target)
  command <- paste(command, collapse = ", ")
  command <- paste0(gather, "(", command, ")")
  return(
    data.frame(target = target, command = command, stringsAsFactors = F)
    )
}

#' @title Function \code{analyses}
#' @description Generate a workflow plan data frame to
#' analyze multiple datasets using multiple methods of analysis.
#' @seealso \code{\link{summaries}},
#'  \code{\link{make}}, \code{\link{plan}}
#' @export
#' @return an evaluated workflow plan data frame of analysis instructions
#' @param plan workflow plan data frame of analysis methods.
#' The commands in the \code{command} column must
#' have the \code{..dataset..} wildcard where the datasets go.
#' For example, one command could be \code{lm(..dataset..)}. Then,
#' the commands in the output will include \code{lm(your_dataset_1)},
#' \code{lm(your_dataset_2)}, etc.
#' @param datasets workflow plan data frame with instructions
#' to make the datasets.
#' @examples
#' datasets <- plan(
#'   small = simulate(5),
#'   large = simulate(50))
#' methods <- plan(
#'   regression1 = reg1(..dataset..),
#'   regression2 = reg2(..dataset..))
#' analyses(methods, datasets = datasets)
analyses <- function(plan, datasets){
  evaluate(
    plan,
    wildcard = "..dataset..",
    values = datasets$target
    )
}

#' @title Function \code{summaries}
#' @description Generate a worklfow plan data frame for summarizing
#' multiple analyses of multiple datasets multiple ways.
#' @seealso \code{\link{analyses}}, \code{\link{make}}, \code{\link{plan}}
#' @export
#' @return an evaluated workflow plan data frame of instructions
#' for computing summaries of analyses and datasets.
#' analyses of multiple datasets in multiple ways.
#' @param plan workflow plan data frame with commands for the summaries.
#' Use the \code{..analysis..} and \code{..dataset..} wildcards
#' just like the \code{..dataset..} wildcard in \code{\link{analyses}()}.
#' @param analyses workflow plan data frame of analysis instructions
#' @param datasets workflow plan data frame with instructions to make
#' or import the datasets.
#' @param gather Character vector, names of functions to gather the
#' summaries. If not \code{NULL}, the length must be the number of
#' rows in the \code{plan}. See the \code{\link{gather}()} function
#' for more.
#' @examples
#' datasets <- plan(
#'   small = simulate(5),
#'   large = simulate(50))
#' methods <- plan(
#'   regression1 = reg1(..dataset..),
#'   regression2 = reg2(..dataset..))
#' analyses <- analyses(methods, datasets = datasets)
#' summary_types <- plan(
#'   summ = summary(..analysis..),
#'   coef = coef(..analysis..))
#' summaries(summary_types, analyses, datasets, gather = NULL)
#' summaries(summary_types, analyses, datasets)
#' summaries(summary_types, analyses, datasets, gather = "list")
#' summaries(summary_types, analyses, datasets, gather = c("list", "rbind"))
summaries <- function(
  plan,
  analyses,
  datasets,
  gather = rep("list", nrow(plan))
  ){
  plan <- with_analyses_only(plan)
  out <- plan
  group <- paste(colnames(out), collapse = "_")
  out[[group]] <- out$target
  if (!any(grepl("..analysis..", out$command, fixed = TRUE))){
    stop(
      "no '..analysis..' wildcard found in plan$command. ",
      "Use analyses() instead."
      )
  }
  out <- evaluate(out, wildcard = "..analysis..", values = analyses$target)
  out <- evaluate(
    out,
    wildcard = "..dataset..",
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
      gather(
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
  has_analysis <- grepl("..analysis..", plan$command, fixed = TRUE)
  if (any(!has_analysis)){
    warning(
      "removing ",
      sum(has_analysis),
      " rows with no ..analysis.. wildcard in the command.",
      "Use analyses() for these."
      )
  }
  return(plan[has_analysis, ])
}

unique_random_string <- function(exclude = NULL, n = 30){
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
