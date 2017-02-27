#' @title Function \code{evaluate}
#' @description Evaluates the wildcard placeholders of a data frame of \code{drake} instructions.
#' Use the \code{\link{help_drake}} function to get more help.
#' @details If \code{wildcard} and \code{values} are not \code{NULL}, the members of 
#' \code{values} will replace \code{wildcard} in the \code{code}
#' column of \code{x}. If the \code{rules} list is not \code{NULL}, \code{rules} readds precedence
#' over \code{wildcard} and \code{values}. In this case, the names of \code{rules}
#' act as wildcards, and each corresponding element of \code{rules} acts as a 
#' \code{values} argument in a recursive call to \code{evaluate}.
#' Use the \code{\link{help_drake}} function to get more help.
#' @seealso \code{\link{help_drake}}
#' @export 
#' @return an evaluated data frame
#' @param x argument data frame
#' @param rules Named list with wildcards as names and vectors of replacements
#' as values. This is a way to evaluate multiple wildcards at once.
#' @param wildcard character string to replace with elements of \code{values}.
#' @param values values to replace the wildcard in the drake instructions. Must be
#' the same length as \code{x$code} if \code{expand} is \code{TRUE}.
#' @param expand If \code{TRUE}, loop over \code{values} when evaluating the wildcard,
#' creating more rows in the output data frame. Otherwise, each occurance of the wildcard
#' is replaced with the next entry in the \code{values} vector, and the values are recycled.
evaluate = function(x, rules = NULL, wildcard = NULL, values = NULL, expand = TRUE){
  x = sanitize(x)
  if(!is.null(rules)) return(evaluations(x = x, rules = rules, expand = expand))
  if(is.null(wildcard) | is.null(values)) return(x)
  matches = grepl(wildcard, x$code, fixed = TRUE)
  if(!any(matches)) return(x)
  major = unique_random_string(colnames(x))
  minor = unique_random_string(c(colnames(x), major))
  x[[major]] = x[[minor]] = 1:nrow(x)
  y = x[matches,]
  if(expand) y = expand(y, values)
  values = rep(values, length.out = dim(y)[1])
  y$code = Vectorize(function(value, code) gsub(wildcard, value, code, fixed = TRUE))(values, y$code)
  rownames(x) = rownames(y) = NULL
  y[[minor]] = 1:nrow(y)
  out = rbind(y, x[!matches,])
  out = out[order(out[[major]], out[[minor]]),]
  out[[major]] = out[[minor]] = NULL
  rownames(out) = NULL
  sanitize(out)
}

# evaluate multiple wildcards
evaluations = function(x, rules = NULL, expand = TRUE){
  if(is.null(rules)) return(x)
  stopifnot(is.list(rules))
  for(i in 1:length(rules))
    x = evaluate(x, wildcard = names(rules)[i], values = rules[[i]], expand = expand)
  x
}

#' @title Function \code{expand}
#' @description Expands a dataframe of drake instructions by duplicating rows.
#' Use the \code{\link{help_drake}} function to get more help.
#' @details Use the \code{\link{help_drake}} function to get more help.
#' @seealso \code{\link{help_drake}}
#' @export 
#' @return an expanded data frame
#' @param x argument data frame
#' @param values values to expand over
expand = function(x, values = NULL){
  x = sanitize(x)
  if(!length(values)) return(x)
  d1 = each = dim(x)[1]
  x = x[rep(1:dim(x)[1], each = length(values)),]
  values = rep(values, times = d1)
  x$output = paste(x$output, values, sep = "_")
  row.names(x) = NULL
  sanitize(x)
}

#' @title Function \code{gather}
#' @description Aggregate/gather the outputs of a previous set of drake instructions.
#' Use the \code{\link{help_drake}} function to get more help.
#' @details Use the \code{\link{help_drake}} function to get more help.
#' @seealso \code{\link{help_drake}}
#' @export 
#' @return data frame with a code to gather the outputs in \code{x}
#' @param x argument data frame
#' @param output name of aggregated output object
#' @param gather function used to gather the outputs
gather = function(x = NULL, output = "output", gather = "list"){
  x = sanitize(x)
  code = paste(x$output, "=", x$output)
  code = paste(code, collapse = ", ")
  code = paste0(gather, "(", code, ")")
  data.frame(output = output, code = code, stringsAsFactors = F) %>%
    sanitize
}

#' @title Function \code{analyses}
#' @description Generate a portion of a \code{drake} workflow for
#' analyzing multiple datasets with multiple methods.
#' Use the \code{\link{help_drake}} function to get more help.
#' @details Use the \code{\link{help_drake}} function to get more help.
#' @seealso \code{\link{help_drake}}, \code{\link{summaries}},
#'  \code{\link{make}}, \code{\link{plan}}, \code{\link{example_plan}}
#' @export 
#' @return data frame of instructions for analyses (wildcards unevaluated)
#' @param plan data frame with unevaluated wildcards
#' @param datasets dataframe with the drake workflow for datasets
analyses = function(plan, datasets){
  evaluate(plan, wildcard = "..dataset..", values = datasets$output)
}

#' @title Function \code{summaries}
#' @description Generate a portion of a \code{drake} workflow
#' for summarizing multiple anlayses of multiple datasets in multiple ways.
#' Use the \code{\link{help_drake}} function to get more help.
#' @details Use the \code{\link{help_drake}} function to get more help.
#' @seealso \code{\link{help_drake}}, \code{\link{analyses}},
#'  \code{\link{make}}, \code{\link{plan}}, \code{\link{example_plan}}
#' @export 
#' @return portion of a \code{drake} workflow for summarizing multiple
#' analyses of multiple datasets in multiple ways
#' @param plan data frame of instructions for summaries (wildcards unevaluated)
#' @param analyses data frame of analysis instructions
#' @param datasets data frame of dataset instructions
#' @param gather Character vector, names of functions to gather the summaries.
#' If not \code{NULL}, length must be the number of rows in the \code{plan}
#' argument.
summaries = function(plan, analyses, datasets, gather = rep("list", dim(plan)[1])){
  out = plan
  group = paste(colnames(out), collapse = "_")
  out[[group]] = out$output
  out = evaluate(out, wildcard = "..analysis..", values = analyses$output)
  out = evaluate(out, wildcard = "..dataset..", values = datasets$output, expand = FALSE)
  if(is.null(gather)) return(out[setdiff(names(out), group)])
  top = ddply(out, group, function(x){
    y = x[[group]][1]
    gather(x, output = y, gather = gather[which(y == plan$output)])
  })
  out[[group]] = top[[group]] = NULL
  rbind(top, out) %>% sanitize
}

unique_random_string = function(exclude = NULL, n = 30){
  while((out <- stri_rand_strings(1, n)) %in% exclude) next
  out
}
