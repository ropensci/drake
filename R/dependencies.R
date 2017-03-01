dependencies = function(targets, args){
  adjacent_vertices(graph = args$graph, v = targets, mode = "in") %>%
    lapply(FUN = names) %>% unlist %>% unique %>% unname
}

command_dependencies = function(command){
  if(!length(command)) return()
  if(is.na(command)) return()
  command = as.character(command)
  fun = function(){}
  body(fun) = parse(text = command)
  non_files = function_dependencies(fun) %>% unlist 
  body(fun) = parse(text = expose_filenames(command))
  with_files = function_dependencies(fun) %>% unlist
  files = setdiff(with_files, non_files)
  if(length(files)) files = quotes(files, single = TRUE)
  c(non_files, files) %>% unique %>% unname
}

import_dependencies = function(object){
  if(is.function(object)) 
    function_dependencies(object) %>% unlist %>% unname
  else
    character(0)
}

function_dependencies = function(funct){
  findGlobals(funct, merge = FALSE) %>% parsable_list 
}

parsable_list = function(x){
  lapply(x, function(y) Filter(is_parsable, y))
}

is_parsable = Vectorize(function(x){
  tryCatch({parse(text = x); TRUE}, error = function(e) FALSE)
}, "x")

expose_filenames = Vectorize(function(x){
  gsub("'", "", x)
}, "x")

is_file = function(x){
  grepl("^'", x) & grepl("'$", x)
}

is_not_file = function(x){
  !is_file(x)
}
