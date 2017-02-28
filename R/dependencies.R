dependencies = function(targets, args){
  adjacent_vertices(graph = args$graph, v = targets, mode = "in") %>%
    lapply(FUN = names) %>% unlist %>% unique %>% unname
}

code_dependencies = Vectorize(function(x){
  if(is.character(x) | is.factor(x)) out = command_dependencies(x)
  else if(is.function(x)) out = function_dependencies(x)
  else return()
  if(length(out$files)) out$files = quotes(out$files, single = TRUE)
  unlist(out) %>% unname
}, "x", SIMPLIFY = FALSE, USE.NAMES = FALSE)

command_dependencies = function(x){
  if(!length(x)) return()
  x = as.character(x)
  fun = function(){}
  body(fun) = parse(text = x)
  dep1 = function_dependencies(fun) 
  body(fun) = parse(text = expose_file_dependencies(x))
  dep2 = function_dependencies(fun)
  dep1$files = setdiff(dep2$variables, dep1$variables)
  dep1
}

function_dependencies = function(x){
  findGlobals(x, merge = FALSE) %>% parsable_list
}

parsable_list = function(x){
  lapply(x, function(y) Filter(is_parsable, y))
}

is_parsable = Vectorize(function(x){
  tryCatch({parse(text = x); TRUE}, error = function(e) FALSE)
}, "x")

expose_file_dependencies = Vectorize(function(x){
  gsub("'", "", x)
}, "x")

is_file = function(x){
  grepl("^'", x) & grepl("'$", x)
}

is_not_file = function(x){
  !is_file(x)
}
