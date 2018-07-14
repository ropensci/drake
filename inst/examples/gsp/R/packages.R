# Here, load the packages you need for your workflow.

library(drake)
library(Ecdat) # econometrics datasets
library(knitr)
library(ggplot2)

pkgconfig::set_config("drake::strings_in_dots" = "literals")
