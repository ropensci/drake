library(drake)
saveRDS(1:10, file = "my_file.rds")
my_plan <- data.frame(
  target = "x",
  command = "readRDS('my_file.rds')"
)
make(my_plan, verbose = FALSE)
file.rename(from = ".drake", to = "cache")
