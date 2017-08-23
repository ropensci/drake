#' @title Function deps
#' @description List the dependencies of a function or workflow plan command. 
#' @export
#' @param x Either a function or a string.  
#' Strings are commands from your workflow plan data frame.
#' @return names of dependencies. Files wrapped in single quotes.
#' The other names listed are functions or generic objects.
#' @examples
#' f <- function(x, y){
#'   out <- x + y + g(x)
#'   saveRDS(out, 'out.rds')
#' }
#' deps(f)
#' my_plan <- plan(
#'   x = 1 + some_object,
#'   my_target = x + readRDS('tracked_input_file.rds'),
#'   return_value = f(x, y, g(z + w))
#' )
#' deps(my_plan$command[1])
#' deps(my_plan$command[2])
#' deps(my_plan$command[3])
deps <- function(x){
  if (is.function(x)){
    out <- function_dependencies(x)
  } else if (is.character(x)){
    out <- command_dependencies(x)
  } else{
    stop("x must be a character scalar or function.")
  }
  clean_dependency_list(out)
}

dependencies <- function(targets, config){
  adjacent_vertices(
    graph = config$graph,
    v = targets,
    mode = "in"
    ) %>%
  lapply(FUN = names) %>%
  clean_dependency_list()
}

command_dependencies <- function(command){
  if (!length(command)){
    return()
  }
  if (is.na(command)){
    return()
  }
  command <- as.character(command) %>%
    braces()
  fun <- function(){} # nolint: I'm still not sure why these braces need to be here.
  body(fun) <- parse(text = command)
  non_files <- function_dependencies(fun) %>%
    unlist()
  files <- extract_filenames(command)
  if (length(files)){
    files <- quotes(files, single = TRUE)
  }
  c(non_files, files) %>%
    clean_dependency_list()
}

import_dependencies <- function(object){
  if (is.function(object)){
    function_dependencies(object) %>% clean_dependency_list
  } else{
    character(0)
  }
}

# Walk through function f and find `pkg::fun()` and `pkg:::fun()` calls.
find_namespaced_functions <- function(f, found = character(0)){
  if (is.function(f)){
    return(find_namespaced_functions(body(f), found))
  } else if (is.call(f) && deparse(f[[1]]) %in% c("::", ":::")){
    found <- c(found, deparse(f))
  } else if (is.recursive(f)){
    v <- lapply(as.list(f), find_namespaced_functions, found)
    found <- unique(c(found, unlist(v)))
  }
  found
}

is_vectorized <- function(funct){
  if (!is.function(funct)){
    return(FALSE)
  }
  vectorized_names <- "FUN" # Chose not to include other names.
  if (!all(vectorized_names %in% ls(environment(funct)))){
    return(FALSE)
  }
  f <- environment(funct)[["FUN"]]
  is.function(f)
}

function_dependencies <- function(funct){
  if (typeof(funct) != "closure"){
    funct <- function(){} # nolint: curly braces are necessary
  }
  if (is_vectorized(funct)){
    funct <- environment(funct)[["FUN"]]
  }
  out <- findGlobals(funct, merge = FALSE)
  namespaced <- find_namespaced_functions(funct)
  out$functions <- c(out$functions, namespaced) %>%
    sort()
  parsable_list(out)
}

clean_dependency_list <- function(x){
  x %>%
    unlist() %>%
    unname() %>%
    unique() %>%
    sort()
}

parsable_list <- function(x){
  lapply(x, function(y) Filter(is_parsable, y))
}

is_parsable <- Vectorize(function(x){
  tryCatch({
    parse(text = x); TRUE
  },
  error = function(e) FALSE
  )
    },
  "x")

extract_filenames = function(command){
  if(!safe_grepl("'", command)) return(character(0))
  splits = str_split(command, "'")[[1]]
  splits[seq(from = 2, to = length(splits), by = 2)]
}

safe_grepl = function(pattern, x){
  tryCatch(grepl(pattern, x), error = function(e) FALSE)
}

is_file = function(x){
  safe_grepl("^'", x) & safe_grepl("'$", x)
}

is_not_file = function(x){
  !is_file(x)
}
