# Load your packages in the usual way.

library(drake)
library(tidyverse)
pkgconfig::set_config("drake::strings_in_dots" = "literals") # For convenience

# Your custom code is a bunch of functions.

create_plot <- function(data) {
  ggplot(data, aes(x = Petal.Width, fill = Species)) +
    geom_histogram(binwidth = 0.25) +
    theme_gray(20)
}

# The workflow plan data frame outlines what you are going to do.

plan <- drake_plan(
  raw_data = readxl::read_excel(file_in("raw_data.xlsx")),
  data = raw_data %>%
    mutate(Species = forcats::fct_inorder(Species)) %>%
    select(-X__1),
  hist = create_plot(data),
  fit = lm(Sepal.Width ~ Petal.Width + Species, data),
  rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)

# Run your work with make().

make(plan)

# See also loadd(), readd(), vis_drake_graph(), and drake_config(). # nolint
