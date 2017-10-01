library(drake)
unlink("cache", recursive = TRUE)
saveRDS(1:10, file = "my_file.rds")
my_plan <- data.frame(
  target = "x",
  command = "eply::strings(my_function('my_file.rds'))"
)
my_function <- function(x){
  x
}
make(my_plan)
file.rename(from = ".drake", to = "cache")
