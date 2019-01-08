devtools::load_all()
for (scenario in testing_scenario_names()){
  cat(scenario, "\n")
  system2(
    command = "R",
    args = paste0(
      "CMD BATCH --no-save '--args ",
      scenario, "' one.R ", scenario, ".out"
    )
  )
}
