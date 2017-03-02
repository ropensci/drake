fix_deprecated_plan_names = function(plan){
  if(any(colnames(plan) %in% c("output", "code")))
    warning("Drake is no longer using \"output\" or \"code\" ",
      "for column names in workflow plan data frames. Use \"target\" ",
      "and \"command\" instead.")
  colnames(plan) = gsub("^output$", "target", colnames(plan)) %>%
    gsub(pattern = "^code$", replacement = "command")
  plan
}
