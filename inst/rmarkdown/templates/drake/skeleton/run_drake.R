# Load your packages in the usual way.

library(drake)
library(tidyverse)

# Your custom code is a bunch of functions.

get_data <- function(){
  iris
}

create_plot <- function(data) {
  ggplot(data, aes(x = Petal.Width, fill = Species)) +
    geom_histogram()
}

# The workflow plan is a data frame.
# It outlines the steps of the workflow but does not run it.

plan <- drake_plan(
  raw_data = get_data(),
  data = raw_data %>%
    mutate(Species = forcats::fct_inorder(Species)),
  hist = create_plot(data),
  fit = lm(Sepal.Width ~ Petal.Width + Species, data),
  rmarkdown::render(
    knitr_in("skeleton.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)

# The make() function actually runs the workflow
# and generates report.html.

make(plan)
