# Load your packages in the usual way.

library(drake)
library(tidyverse)
pkgconfig::set_config("drake::strings_in_dots" = "literals") # For convenience

# Your custom code is a bunch of functions.

create_plot <- function(data) {
  ggplot(data) +
    geom_histogram(aes(x = Ozone), binwidth = 10) +
    theme_gray(24)
}

# The workflow plan data frame outlines what you are going to do.

plan <- drake_plan(
  raw_data = readxl::read_excel(file_in("raw_data.xlsx")),
  data = raw_data %>%
    mutate(Ozone = replace_na(Ozone, mean(Ozone, na.rm = TRUE))),
  hist = create_plot(data),
  fit = lm(Ozone ~ Wind + Temp, data),
  report = rmarkdown::render(
    knitr_in("report.Rmd"),
    output_file = file_out("report.html"),
    quiet = TRUE
  )
)

# Run your work with make().

make(plan)

# See also loadd(), readd(), vis_drake_graph(), and drake_config(). # nolint
