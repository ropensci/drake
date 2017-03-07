dependencies = function(targets, config){
  adjacent_vertices(graph = config$graph, v = targets, mode = "in") %>%
    lapply(FUN = names) %>% clean_dependency_list
}

command_dependencies = function(command){
  if(!length(command)) return()
  if(is.na(command)) return()
  command = as.character(command) %>% braces
  fun = function(){}
  body(fun) = parse(text = command)
  non_files = function_dependencies(fun) %>% unlist 
  files = extract_filenames(command)
  if(length(files)) files = quotes(files, single = TRUE)
  c(non_files, files) %>% clean_dependency_list
}

import_dependencies = function(object){
  if(is.function(object)) 
    function_dependencies(object) %>% clean_dependency_list
  else
    character(0)
}

function_dependencies = function(funct){
  findGlobals(funct, merge = FALSE) %>% parsable_list
}

clean_dependency_list = function(x){
  x %>% unlist %>% unname %>% unique %>% sort
}

parsable_list = function(x){
  lapply(x, function(y) Filter(is_parsable, y))
}

is_parsable = Vectorize(function(x){
  tryCatch({parse(text = x); TRUE}, error = function(e) FALSE)
}, "x")

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
