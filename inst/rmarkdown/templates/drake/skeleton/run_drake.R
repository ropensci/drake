# Load your packages in the usual way.

library(drake)
library(tidyverse)
pkgconfig::set_config("drake::strings_in_dots" = "literals")

# Your custom code is a bunch of functions.

create_plot <- function(data) {
  ggplot(data, aes(x = Petal.Width, fill = Species)) +
    geom_histogram() +
    geom_histogram(binwidth = 0.25) +
    theme_gray(20)
}

# The workflow plan is a data frame.
# It outlines the steps of the workflow but does not run it.

plan <- drake_plan(
  raw_data = readxl::read_excel(file_in("raw_data.xlsx")),
  data = raw_data %>%
    mutate(Species = forcats::fct_inorder(Species)) %>%
    select(-X__1),
  hist = create_plot(data),
  fit = lm(Sepal.Width ~ Petal.Width + Species, data),
  rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.pdf"),
    quiet = TRUE
  )
)

# The make() function actually runs the workflow
# and generates report.pdf.

make(plan)
