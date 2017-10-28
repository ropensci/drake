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

plot_graph(
  my_plan, file = "fromout.html", selfcontained = TRUE,
  width = "100%", height = "500px",
  from = c("large", "small")
)

plot_graph(
  my_plan, file = "fromin.html", selfcontained = TRUE,
  width = "100%", height = "500px",
  from = "small", mode = "in"
)

plot_graph(
  my_plan, file = "fromall.html", selfcontained = TRUE,
  width = "100%", height = "500px",
  from = "small", mode = "all", order = 1
)

plot_graph(
  my_plan, file = "shrink.html", selfcontained = TRUE,
  width = "100%", height = "500px",
  from = "small", mode = "all", order = 1,
  shrink_edges = TRUE
)

clean(destroy = TRUE)
unlink("report.Rmd")
