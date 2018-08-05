print.drake_plan <- function(x, ...){
  if (requireNamespace("styler", quietly = TRUE)){
    NextMethod()
    # print_drake_plan(x, ...) # under development # nolint
  } else {
    NextMethod() # nocov
  }
}

print_drake_plan <- function (x, width = getOption("width"), ...){
  drake_plan_call(x) %>%
    rlang::expr_text(width = 70) %>%
    styler::style_text() %>%
    print()
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
  if (!is.null(args[["command"]]) && is.character(args[["command"]])){
    args$command <- parse(text = args[["command"]])[[1]]
  }
  if (!is.null(args[["trigger"]]) && is.character(args[["trigger"]])){
    args$trigger <- parse(text = args[["trigger"]])[[1]]
  }
  if (!identical(names(args), "command")){
    args[["command"]] <- as.call(c(quote(target), args))
  }
  args[["command"]]
}
