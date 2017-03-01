#' @title Deprecated function \code{make}.
#' @description Use \code{link{run}()} instead.
#' @seealso \code{\link{run}}
#' @export
#' @param ... arguments to \code{\link{run}()}
make = function(...){
  .Deprecated("run", package = "drake", old = "make", 
    msg = paste("Function 'make()' is deprecated. Use 'run()' instead.",
      "See help(\"run\") for details.")
  run(...)
}

fix_deprecated_plan_names = function(plan){
  if(any(colnames(plan) %in% c("output", "code")))
    warning("Drake is no longer using \"output\" or \"code\" ",
      "for column names in workflow plan data frames. Use \"target\" ",
      "and \"command\" instead.")
  colnames(plan) = gsub("^output$", "target", colnames(plan)) %>%
    gsub(pattern = "^code$", replacement = "command")
  plan
}
