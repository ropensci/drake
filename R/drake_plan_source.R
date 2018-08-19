#' @title Get the code to produce a given workflow plan data frame
#' @description You supply a plan, and [drake_plan_source()]
#'   supplies code to generate that plan. If you have the
#'   [`prettycode` package](https://github.com/r-lib/prettycode),
#'   installed, you also get nice syntax highlighting in the console
#'   when you print it.
#' @export
#' @seealso [drake_plan()]
#' @return a character vector of lines of text. This text
#'   is a call to [drake_plan()] that produces the plan you provide.
#' @param plan a workflow plan data frame (see [drake_plan()])
#' @examples
#' plan <- drake::drake_plan(
#'   small_data = download_data("https://some_website.com") %>%
#'     select_my_columns() %>%
#'     munge(),
#'   large_data_raw = target(
#'     command = download_data("https://lots_of_data.com") %>%
#'       select_top_columns,
#'     trigger = trigger(
#'       change = time_last_modified("https://lots_of_data.com"),
#'       command = FALSE,
#'       depend = FALSE
#'     ),
#'     timeout = 1e3
#'   )
#' )
#' print(plan)
#' source <- drake_plan_source(plan)
#' print(source) # Install the prettycode package for syntax highlighting.
#' \dontrun{
#' test_with_dir("suppress side effects", {
#' writeLines(source, "my_script.R") # Save the code to an R script.
#' })
#' }
drake_plan_source <- function(plan){
  assert_pkg("styler")
  text <- drake_plan_call(plan) %>%
    rlang::expr_text() %>%
    styler::style_text(style = drake_plan_source_style)
  class(text) <- c("drake_plan_source", class(text))
  text
}

drake_plan_call <- function(plan){
  target_calls <- purrr::pmap(plan, drake_target_call) %>%
    setNames(plan$target)
  as.call(c(quote(drake_plan), target_calls))
}

drake_target_call <- function(...){
  args <- list(...)[drake_plan_columns()] %>%
    select_valid()
  target <- parse(text = args$target)[[1]]
  args$target <- NULL
  if (is.character(args[["command"]])){
    args$command <- parse(text = args[["command"]])[[1]]
  }
  if ("trigger" %in% names(args)){
    if (is.character(args[["trigger"]])){
      args[["trigger"]] <- parse(text = args[["trigger"]])[[1]]
    }
  }
  if (!identical(names(args), "command")){
    args[["command"]] <- as.call(c(quote(target), args))
  }
  args[["command"]]
}

drake_plan_source_style <- function(){
  styler::create_style_guide(
    line_break = tibble::lst(add_line_breaks)
  )
}

add_line_breaks <- function(pd_flat){
  is_symbol <- pd_flat$token == "SYMBOL_SUB"
  pd_flat$lag_newlines <- pd_flat$lag_newlines | is_symbol
  pd_flat$indent <- pd_flat$indent + 2 * is_symbol
  pd_flat
}

#' @export
print.drake_plan_source <- function(x, ...){
  if (requireNamespace("prettycode", quietly = TRUE)){
    x <- prettycode::highlight(x)
    NextMethod()
  } else {
    NextMethod() # nocov
  }
}
