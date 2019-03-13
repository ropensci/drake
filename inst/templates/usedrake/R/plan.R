# The drake plan outlines what you are going to do.
# It declares all your targets and puts the commands in a data frame.
# It does not actually build anything. To build your targets,
# see make() and r_make().

# plan <- drake_plan(
#   raw_data = datasets::iris,
#   data = raw_data %>%
#     mutate(Species = forcats::fct_inorder(Species)),
#   hist = create_plot(data),
#   fit = lm(Sepal.Width ~ Petal.Width + Species, data)
# )
