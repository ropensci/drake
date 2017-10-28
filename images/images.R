# Generate the HTML widgets in the images/ folder.
# These interactive graphs are embedded in the vignettes.
devtools::load_all() # load current drake
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
  my_plan, file = "targetsonly.html", selfcontained = TRUE,
  targets_only = TRUE,
  width = "100%", height = "500px",
  from = c("large", "small")
)

plot_graph(
  my_plan, file = "fromout.html", selfcontained = TRUE,
  width = "100%", height = "500px",
  from = c("regression2_small", "regression2_large")
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

small_plan <- workplan(a = 1, b = f(2))
f <- function(x){
  x
}

plot_graph(small_plan, file = "small_local.html", selfcontained = TRUE,
  width = "100%", height = "500px")

plot_graph(small_plan, file = "small_distributed.html", selfcontained = TRUE,
  width = "100%", height = "500px", parallelism = "future_lapply")

clean(destroy = TRUE)
unlink("report.Rmd")
