# Generate the HTML widgets in the images/ folder.
# These interactive graphs are embedded in the vignettes.
devtools::load_all() # load drake
clean(destroy = TRUE)
load_basic_example()
plot_graph(my_plan, file = "outdated.html", selfcontained = TRUE,
  width = "100%", height = "500px")
make(my_plan)
plot_graph(my_plan, file = "built.html", selfcontained = TRUE,
  width = "100%", height = "500px")
reg2 <- function(d){
  d$x3 <- d$x ^ 3
  lm(y ~ x3, data = d)
}
plot_graph(my_plan, file = "reg2.html", selfcontained = TRUE,
  width = "100%", height = "500px")
clean(destroy = TRUE)
unlink("report.Rmd")
