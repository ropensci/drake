cache_macro <- "DRAKE_CACHE"
cache_value_macro <- paste0("$(", cache_macro, ")")

recipe_command_macro <- "DRAKE_RECIPE_COMMAND"
recipe_command_value_macro <- paste0("$(", recipe_command_macro, ")")

globalenv_file <- function(cache_path){
  file.path(cache_path, "globalenv.RData")
}

to_unix_path <- function(x){
  gsub("\\\\", "/", x)
}
