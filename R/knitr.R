
# TO DO:
# 1. Notice namespaced calls: drake::readd(), drake:::loadd(), etc.
# 2. Look for strings? (possible file targets)


knitr_dependencies <- function(target){
  file <- unquote(target)
  fragments <- get_tangled_frags(file)
  find_knitr_targets(fragments)
}

# From https://github.com/duncantl/CodeDepends/blob/master/R/sweave.R#L15
get_tangled_frags <- function(doc, txt = readLines(doc)) {
  in.con <- textConnection(txt)
  out.con <- textConnection("bob", "w", local = TRUE)
  on.exit({
    close(in.con)
    close(out.con)
  })
  knitr::knit(in.con, output = out.con, tangle = TRUE, quiet = TRUE)
  code <- textConnectionValue(out.con)
  parse(text = code)
}

find_knitr_targets <- function(expr, targets = character(0)){
  if (is.function(expr)){
    return(find_knitr_targets(body(expr), targets = targets))
  } else if (is.call(expr) & length(expr) > 1){
    targets <- c(targets, analyze_loadd(expr), analyze_readd(expr))
    targets <- as.list(expr) %>%
      Filter(f = found_loadd_readd) %>%
      lapply_find_knitr_targets(targets = targets)
  } else if (is.recursive(expr)){
    targets <- lapply_find_knitr_targets(
      list = as.list(expr), targets = targets)
  }
  targets
}

analyze_loadd <- function(expr){
  if (!grepl("loadd$", deparse(expr[[1]]))){
    return()
  }
  args <- as.list(expr)[-1]
  targets <- unnamed_in_list(args)
  list <- get_specific_arg(args = args, name = "list")
  c(targets, list)
}

analyze_readd <- function(expr){
  if (!grepl("readd$", deparse(expr[[1]]))){
    return()
  }
  args <- as.list(expr)[-1]
  targets <- unnamed_in_list(args)
  target <- get_specific_arg(args = args, name = "target")
  c(targets, target)
}

found_loadd_readd <- function(x){
  grepl("readd|loadd", wide_deparse(x))
}

get_specific_arg <- function(args, name){
  tryCatch(
    eval(args[[name]]),
    error = function(e){
      character(0)
    }
  )
}

lapply_find_knitr_targets <- function(list, targets){
  v <- lapply(list, find_knitr_targets, targets = targets)
  targets <- unique(c(targets, unlist(v)))
}

unnamed_in_list <- function(x){
  if (!length(names(x))){
    as.character(x)
  } else {
    as.character(x[!nchar(names(x))])
  }
}
