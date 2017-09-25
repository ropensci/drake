cache_macro <- "DRAKE_CACHE"
cache_value_macro <- paste0("$(", cache_macro, ")")

globalenv_file <- function(cache_path){
  file.path(cache_path, "globalenv.RData")
}

to_unix_path <- function(x){
  gsub("\\\\", "/", x)
}
