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
  assert_input_files_exist(args)
  check_strings(args$plan)
  invisible()
}

check_args = function(args){
  stopifnot(is.data.frame(args$plan))
  if(!all(c("target", "command") %in% colnames(args$plan)))
    stop("The columns of your workflow plan data frame ",
      "must include 'target' and 'command'.")
  stopifnot(nrow(args$plan) > 0)
  stopifnot(length(args$targets) > 0)
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
    "Remember: single-quoted strings are file target dependencies", 
    "and double-quoted strings are just ordinary strings.",
    sep = "\n")
  for(target in seq_len(length(x))){
    cat("\ntarget:", names(x)[target], "\n")
    strings = paste0("  \"", x[[target]], "\"") %>% paste(collapse = "\n")
    cat("strings in command:\n", strings, "\n", sep = "")
  }
}

assert_input_files_exist = function(args){
  missing_files = next_targets(args$graph) %>% Filter(f = is_file) %>% 
    unquote %>% Filter(f = function(x) !file.exists(x))
  if(length(missing_files)){
    msg = paste0("  ", missing_files) %>% paste(collapse = "\n")
    stop("missing input files:\n", msg)
  }
}
