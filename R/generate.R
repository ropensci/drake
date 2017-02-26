#' @title Function \code{evaluate}
#' @description Evaluates the wildcard placeholders of a data frame of \code{skipit} commands.
#' @details If \code{wildcard} and \code{values} are not \code{NULL}, the members of 
#' \code{values} will replace \code{wildcard} in the \code{command}
#' column of \code{x}. If the \code{rules} list is not \code{NULL}, \code{rules} takes precedence
#' over \code{wildcard} and \code{values}. In this case, the names of \code{rules}
#' act as wildcards, and each corresponding element of \code{rules} acts as a 
#' \code{values} argument in a recursive call to \code{evaluate}.
#' @export 
#' @return an evaluated data frame
#' @param x argument data frame
#' @param rules Named list with wildcards as names and vectors of replacements
#' as values. This is a way to evaluate multiple wildcards at once.
#' @param wildcard character string to replace with elements of \code{values}.
#' @param values values to replace the wildcard in the skipit commands. Must be
#' the same length as \code{x$command} if \code{expand} is \code{TRUE}.
#' @param expand If \code{TRUE}, loop over \code{values} when evaluating the wildcard,
#' creating more rows in the output data frame. Otherwise, each occurance of the wildcard
#' is replaced with the next entry in the \code{values} vector, and the values are recycled.
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

#' @title Function \code{expand}
#' @description Expands a dataframe of skipit commands by duplicating rows.
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

#' @title Function \code{gather}
#' @description Aggregate/gather the targets of a previous set of skipit commands.
#' @export 
#' @return data frame with a command to gather the targets in \code{x}
#' @param x argument data frame
#' @param target name of aggregated output object
#' @param gather function used to gather the targets
gather = function(x, target = "target", gather = "list"){
  command = paste(x$target, "=", x$target)
  command = paste(command, collapse = ", ")
  command = paste0(gather, "(", command, ")")
  data.frame(target = target, command = command, stringsAsFactors = F)
}

#' @title Function \code{analyses}
#' @description Produces a data frame of \code{skipit} analysis commands.
#' @export 
#' @return Preprocessed data frame of \code{skipit} analysis commands
#' @param commands output of the \code{\link{workflow}} function.
#' @param datasets Data frame of commands to generate datasets
analyses = function(commands, datasets){
  evaluate(commands, wildcard = "..dataset..", values = datasets$target)
}

#' @title Function \code{summaries}
#' @description Produces a data frame of \code{skipit} summary commands.
#' @export 
#' @return preprocessed data frame of \code{skipit} summary commands
#' @param commands Data frame output of the \code{\link{workflow}} function.
#' @param analyses Data frame of \code{skipit} commands to generate analyses.
#' @param datasets Data frame of \code{skipit} commands to generate datasets.
#' @param gather Character vector, names of functions to gather the summaries.
#' If not \code{NULL}, length must be the number of rows in the \code{workflow}
#' argument.
summaries = function(commands, analyses, datasets, gather = rep("list", dim(commands)[1])){
  out = commands
  group = paste(colnames(out), collapse = "_")
  out[[group]] = out$target
  out = evaluate(out, wildcard = "..analysis..", values = analyses$target)
  out = evaluate(out, wildcard = "..dataset..", values = datasets$target, expand = FALSE)
  if(is.null(gather)) return(out[setdiff(names(out), group)])
  top = ddply(out, group, function(x){
    y = x[[group]][1]
    gather(x, target = y, gather = gather[which(y == commands$target)])
  })
  out[[group]] = top[[group]] = NULL
  rbind(top, out)
}

evaluations = function(x, rules = NULL, expand = TRUE){
  if(is.null(rules)) return(x)
  stopifnot(is.list(rules))
  for(i in 1:length(rules))
    x = evaluate(x, wildcard = names(rules)[i], values = rules[[i]], expand = expand)
  x
}

unique_random_string = function(exclude = NULL, n = 30){
  while((out <- stri_rand_strings(1, n)) %in% exclude) next
  out
}
