#' @title Mark a data frame as a `drake` workflow plan
#' @description Used for pretty printing only (coming soon).
#'   You do not actually have to mark plans as such.
#'   You can keep them as ordinary data frames.
#' @export
#' @keywords internal
#' @param x object to mark as a `drake` plan
#' @param ... other arguments to the method
#' @examples
#' plan <- list(target = "x", command = "get_data()")
#' class(plan)
#' plan <- as_drake_plan(plan)
#' class(plan)
as_drake_plan <- function(x){
  UseMethod("as_drake_plan")
}

as_drake_plan_ <- function(x, ...){
  tibble::new_tibble(x, ..., subclass = "drake_plan")
}

#' @export
`[.drake_plan` <- function(...){
  as_drake_plan_(NextMethod())
}

#' @export
as_drake_plan.data.frame <- as_drake_plan_

#' @export
as_drake_plan.list <- as_drake_plan_

#' @export
as_drake_plan.tbl_df <- as_drake_plan_

#' @export
as.data.frame.drake_plan <- as_drake_plan_

#' @export
as_data_frame.drake_plan <- as_drake_plan_

#' @export
as_tibble.drake_plan <- as_drake_plan_

#' @export
as.tibble.drake_plan <- as_drake_plan_
