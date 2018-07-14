# Here, load the packages you need for your workflow.

library(cranlogs)
library(drake)
library(dplyr)
library(ggplot2)
library(knitr)

pkgconfig::set_config("drake::strings_in_dots" = "literals")
