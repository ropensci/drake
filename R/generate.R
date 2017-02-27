#' @title Function \command{evaluate}
#' @description Evaluates the wildcard placeholders of a data frame of \command{drake} instructions.
#' @details If \command{wildcard} and \command{values} are not \command{NULL}, the members of 
#' \command{values} will replace \command{wildcard} in the \command{command}
#' column of \command{x}. If the \command{rules} list is not \command{NULL}, \command{rules} readds precedence
#' over \command{wildcard} and \command{values}. In this case, the names of \command{rules}
#' act as wildcards, and each corresponding element of \command{rules} acts as a 
#' \command{values} argument in a recursive call to \command{evaluate}.
#' @export 
#' @return an evaluated data frame
#' @param x argument data frame
#' @param rules Named list with wildcards as names and vectors of replacements
#' as values. This is a way to evaluate multiple wildcards at once.
#' @param wildcard character string to replace with elements of \command{values}.
#' @param values values to replace the wildcard in the drake instructions. Must be
#' the same length as \command{x$command} if \command{expand} is \command{TRUE}.
#' @param expand If \command{TRUE}, loop over \command{values} when evaluating the wildcard,
#' creating more rows in the target data frame. Otherwise, each occurance of the wildcard
#' is replaced with the next entry in the \command{values} vector, and the values are recycled.
evaluate = function(x, rules = NULL, wildcard = NULL, values = NULL, expand = TRUE){
  if(!is.null(rules)) return(evaluations(x = x, rules = rules, expand = expand))
  if(is.null(wildcard) | is.null(values)) return(x)
  matches = grepl(wildcard, x$command, fixed = TRUE)
  if(!any(matches)) return(x)
  major = unique_random_string(colnames(x))
  minor = unique_random_string(c(colnames(x), major))
  x[[major]] = x[[minor]] = 1:nrow(x)
  y = x[matches,]
  if(expand) y = expand(y, values)
  values = rep(values, length.out = dim(y)[1])
  y$command = Vectorize(function(value, command) gsub(wildcard, value, command, fixed = TRUE))(values, y$command)
  rownames(x) = rownames(y) = NULL
  y[[minor]] = 1:nrow(y)
  out = rbind(y, x[!matches,])
  out = out[order(out[[major]], out[[minor]]),]
  out[[major]] = out[[minor]] = NULL
  rownames(out) = NULL
  out
}

# evaluate multiple wildcards
evaluations = function(x, rules = NULL, expand = TRUE){
  if(is.null(rules)) return(x)
  stopifnot(is.list(rules))
  for(i in 1:length(rules))
    x = evaluate(x, wildcard = names(rules)[i], values = rules[[i]], expand = expand)
  x
}

#' @title Function \command{expand}
#' @description Expands a dataframe of drake instructions by duplicating rows.
#' @export 
#' @return an expanded data frame
#' @param x argument data frame
#' @param values values to expand over
expand = function(x, values = NULL){
  if(!length(values)) return(x)
  d1 = each = dim(x)[1]
  x = x[rep(1:dim(x)[1], each = length(values)),]
  values = rep(values, times = d1)
  x$target = paste(x$target, values, sep = "_")
  row.names(x) = NULL
  x
}

#' @title Function \command{gather}
#' @description Aggregate/gather the targets of a previous set of drake instructions.
#' @export 
#' @return data frame with a command to gather the targets in \command{x}
#' @param x argument data frame
#' @param target name of aggregated target object
#' @param gather function used to gather the targets
gather = function(x = NULL, target = "target", gather = "list"){
  command = paste(x$target, "=", x$target)
  command = paste(command, collapse = ", ")
  command = paste0(gather, "(", command, ")")
  data.frame(target = target, command = command, stringsAsFactors = F)
}

#' @title Function \command{analyses}
#' @description Generate a portion of a \command{drake} plan for
#' analyzing multiple datasets with multiple methods.
#' @seealso \command{\link{summaries}},
#'  \command{\link{run}}, \command{\link{plan}}
#' @export 
#' @return data frame of instructions for analyses (wildcards unevaluated)
#' @param plan data frame with unevaluated wildcards
#' @param datasets dataframe with the drake plan for datasets
analyses = function(plan, datasets){
  evaluate(plan, wildcard = "..dataset..", values = datasets$target)
}

#' @title Function \command{summaries}
#' @description Generate a portion of a \command{drake} plan
#' for summarizing multiple anlayses of multiple datasets in multiple ways.
#' @seealso \command{\link{analyses}},
#'  \command{\link{run}}, \command{\link{plan}}
#' @export 
#' @return portion of a \command{drake} plan for summarizing multiple
#' analyses of multiple datasets in multiple ways
#' @param plan data frame of instructions for summaries (wildcards unevaluated)
#' @param analyses data frame of analysis instructions
#' @param datasets data frame of dataset instructions
#' @param gather Character vector, names of functions to gather the summaries.
#' If not \command{NULL}, length must be the number of rows in the \command{plan}
#' argument.
summaries = function(plan, analyses, datasets, gather = rep("list", dim(plan)[1])){
  out = plan
  group = paste(colnames(out), collapse = "_")
  out[[group]] = out$target
  out = evaluate(out, wildcard = "..analysis..", values = analyses$target)
  out = evaluate(out, wildcard = "..dataset..", values = datasets$target, expand = FALSE)
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
