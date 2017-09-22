library(drake)
saveRDS(1:10, file = "my_file.rds")
my_plan <- data.frame(
  target = "x",
  command = "readRDS('my_file.rds')"
)
con <- make(my_plan, return_config = TRUE, verbose = FALSE)
clean_out <- c(
  "config",
  "progress",
  "build_times"
)
for (ns in clean_out){
  for (key in con$cache$list(namespace = ns)){
    con$cache$del(key = key, namespace = ns)
  }
  path <- paste0(".drake", "keys", ns)
  unlink(path, recursive = TRUE)
}
