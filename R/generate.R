#' @title Function \code{evaluate}
#' @description The commands in workflow plan data frames can have 
#' wildcard symbols that can stand for datasets, parameters, function 
#' arguments, etc. These wildcards can be evaluated over a set of 
#' possible values using \code{evaluate}. 
#' @details Specify a single wildcard with the \code{wildcard}
#' and \code{values} arguments. In each command, the text in 
#' \code{wildcard} will be replaced by each value in \code{values}
#' in turn. Specify multiple wildcards ith the \code{rules} argument,
#' which overrules \code{wildcard} and \code{values} if 
#' not \code{NULL}. Here, \code{rules} should be a list with wildcards
#' as names and vectors of possible values as list elements.
#' @export 
#' @return a workflow plan data frame with the wildcards evaluated
#' @param x workflow plan data frame, similar to one produced by 
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
#' Must be the same length as \code{x$command} if \code{expand} is 
#' \code{TRUE}.
#' @param expand If \code{TRUE}, create a new rows in the workflow plan 
#' data frame
#' if multiple values are assigned to a single wildcard.
#' If \code{FALSE}, each occurence of the wildcard
#' is replaced with the next entry in the \code{values} vector, 
#' and the values are recycled.
evaluate = function(x, rules = NULL, wildcard = NULL, values = NULL, 
  expand = TRUE){
  if(!is.null(rules)) 
    return(evaluations(x = x, rules = rules, expand = expand))
  if(is.null(wildcard) | is.null(values)) return(x)
  matches = grepl(wildcard, x$command, fixed = TRUE)
  if(!any(matches)) return(x)
  major = unique_random_string(colnames(x))
  minor = unique_random_string(c(colnames(x), major))
  x[[major]] = x[[minor]] = 1:nrow(x)
  y = x[matches,]
  if(expand) y = expand(y, values)
  values = rep(values, length.out = dim(y)[1])
  y$command = Vectorize(function(value, command) 
    gsub(wildcard, value, command, fixed = TRUE))(values, y$command)
  rownames(x) = rownames(y) = NULL
  y[[minor]] = 1:nrow(y)
  out = rbind(y, x[!matches,])
  out = out[order(out[[major]], out[[minor]]),]
  out[[major]] = out[[minor]] = NULL
  rownames(out) = NULL
  out
}

evaluations = function(x, rules = NULL, expand = TRUE){
  if(is.null(rules)) return(x)
  stopifnot(is.list(rules))
  for(i in 1:length(rules))
    x = evaluate(x, wildcard = names(rules)[i], values = rules[[i]], 
      expand = expand)
  x
}

#' @title Function \code{expand}
#' @description Expands a workflow plan data frame by duplicating rows.
#' This generates multiple replicates of targets with the same commands.
#' @export 
#' @return an expanded workflow plan data frame
#' @param x workflow plan data frame
#' @param values values to expand over. These will be appended to
#' the names of the new targets.
expand = function(x, values = NULL){
  if(!length(values)) return(x)
  d1 = each = dim(x)[1]
  x = x[rep(1:dim(x)[1], each = length(values)),]
  values = rep(values, times = d1)
  x$target = paste(x$target, values, sep = "_")
  row.names(x) = NULL
  x
}

#' @title Function \code{gather}
#' @description Create a new workflow plan data frame with a single new 
#' target. This new target is a list, vector, or other aggregate of
#' a collection of existing targets in another workflow plan data frame.
#' @export 
#' @return workflow plan data frame for aggregating prespecified targets
#' @param x workflow plan data frame of prespecified targets
#' @param target name of the new aggregated target
#' @param gather function used to gather the targets. Should be 
#' one of \code{\link{list}(...)}, \code{\link{c}(...)},
#' \code{\link{rbind}(...)}, or similar.
gather = function(x = NULL, target = "target", gather = "list"){
  command = paste(x$target, "=", x$target)
  command = paste(command, collapse = ", ")
  command = paste0(gather, "(", command, ")")
  data.frame(target = target, command = command, stringsAsFactors = F)
}

#' @title Function \code{analyses}
#' @description Generate a workflow plan data frame to 
#' analyze multiple datasets using multiple methods of analysis.
#' @seealso \code{\link{summaries}},
#'  \code{\link{run}}, \code{\link{plan}}
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
analyses = function(plan, datasets){
  evaluate(plan, wildcard = "..dataset..", values = datasets$target)
}

#' @title Function \code{summaries}
#' @description Generate a worklfow plan data frame for summarizing
#' multiple analyses of multiple datasets multiple ways.
#' @seealso \code{\link{analyses}}, \code{\link{run}}, \code{\link{plan}}
#' @export 
#' @return an evaluated workflow plan data frame of instructions
#' for computing summaries of analyses and datasets.
#' analyses of multiple datasets in multiple ways.
#' @param workflow plan data frame with commands for the summaries.
#' Use the \code{..analysis..} and \code{..dataset..} wildcards 
#' just like the \code{..dataset..} wildcard in \code{\link{analyses}()}. 
#' @param analyses workflow plan data frame of analysis instructions
#' @param datasets workflow plan data frame with instructions to make
#' or import the datasets.
#' @param gather Character vector, names of functions to gather the 
#' summaries. If not \code{NULL}, the length must be the number of 
#' rows in the \code{plan}. See the \code{\link{gather}()} function
#' for more.
summaries = function(plan, analyses, datasets, 
  gather = rep("list", dim(plan)[1])){
  out = plan
  group = paste(colnames(out), collapse = "_")
  out[[group]] = out$target
  out = evaluate(out, wildcard = "..analysis..", values = analyses$target)
  out = evaluate(out, wildcard = "..dataset..", values = datasets$target,
    expand = FALSE)
  if(is.null(gather)) return(out[setdiff(names(out), group)])
  top = ddply(out, group, function(x){
    y = x[[group]][1]
    gather(x, target = y, gather = gather[which(y == plan$target)])
  })
  out[[group]] = top[[group]] = NULL
  rbind(top, out)
}

unique_random_string = function(exclude = NULL, n = 30){
  while((out <- stri_rand_strings(1, n)) %in% exclude) next
  out
}
