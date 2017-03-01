#' @title Function \code{check}
#' @description Check a workflow plan, etc. for obvious 
#' errors such as circular dependencies and 
#' missing input files.
#' @seealso \code{link{plan}}, \code{\link{make}}
#' @export
#' @param plan workflow plan data frame, possibly from 
#' \code{\link{plan}()}.
#' @param targets character vector of targets to make
#' @param envir environment containing user-defined functions
check = function(plan, targets = plan$target, envir = parent.frame()){
  force(envir)
  args = setup(plan = plan, targets = targets, envir = envir, 
    verbose = TRUE, jobs = 1, prework = character(0),
    command = character(0), args = character(0))
  check_args(args)
}

check_args = function(args){
  stopifnot(is.data.frame(args$plan))
  if(!all(c("target", "command") %in% colnames(plan)))
    stop("The columns of your workflow plan data frame ",
      "must include 'target' and 'command'.")
  stopifnot(nrow(plan) > 0)
  stopifnot(length(args$targets))
  stopifnot(is.environment(args$envir))
}

find_files = function(args){
  files = args$order %>% Filter(is_file)
  if(!all(file.exists(files))){
    msg = paste(files, collapse = "\n")
    stop("Missing input files:\n", msg)
  }
}

check_strings = function(plan){
  x = stri_extract_all_regex(plan$command, '(?<=").*?(?=")')
  names(x) = plan$target
  x = x[!is.na(x)]
  if(!length(x)) return()
  x = lapply(x, function(y){
    if(length(y) > 2) return(y[seq(from = 1, to = length(y), by = 2)])
    else return(y)
  })
  cat("Double-quoted strings were found in plan$command.",
    "Should these be single-quoted instead?",
    "Remember: single-quoted strings are file dependencies/targets", 
    "and double-quoted strings are just ordinary strings.",
    sep = "\n")
  for(i in 1:length(x)){
    cat("\ntarget:", names(x)[i], "\n")
    cat("strings in command:", paste0("\"", x[[i]], "\""), "\n")
  }
}
