# This is where you write your drake plan.
# Details: https://books.ropensci.org/drake/plans.html

plan <- drake_plan(
  data = generate_data(),
  model = fit_model(data)
)
