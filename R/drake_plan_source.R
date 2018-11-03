#' @title Show the code required to produce a given workflow plan data frame
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
#'   small_data = download_data("https://some_website.com"),
#'   large_data_raw = target(
#'     command = download_data("https://lots_of_data.com"),
#'     trigger = trigger(
#'       change = time_last_modified("https://lots_of_data.com"),
#'       command = FALSE,
#'       depend = FALSE
#'     ),
#'     timeout = 1e3
#'   ),
#'   strings_in_dots = "literals"
#' )
#' print(plan)
#' if (requireNamespace("styler", quietly = TRUE)){
#'   source <- drake_plan_source(plan)
#'   print(source) # Install the prettycode package for syntax highlighting.
#' }
#' \dontrun{
#' file <- tempfile() # Path to an R script to contain the drake_plan() call.
#' writeLines(source, file) # Save the code to an R script.
#' }
drake_plan_source <- function(plan){
  assert_pkg("styler")
  text <- drake_plan_call(plan)
  text <- style_recursive(text, name = "", append_comma = FALSE)
  class(text) <- c("drake_plan_source", "vertical", "character")
  text
}

drake_plan_call <- function(plan){
  # TODO: when we remove the old file API, remove
  # this hack that converts to the new file API.
  plan <- dplyr::bind_rows(
    plan,
    data.frame(
      target = "strings_in_dots",
      command = "\"literals\"",
      stringsAsFactors = FALSE
    )
  )
  target_calls <- purrr::pmap(plan, drake_target_call)
  names(target_calls) <- plan$target
  as.call(c(quote(drake_plan), target_calls))
}

drake_target_call <- function(...){
  args <- select_valid(list(...))
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

style_recursive <- function(expr, name, append_comma){
  text <- style_recursive_loop(expr)
  head <- character(0)
  if (nzchar(name)){
    head <- paste(name, "= ")
  }
  head <- paste0(head, wide_deparse(expr[[1]]), "(")
  out <- c(head, paste0("  ", text), ")")
  if (append_comma){
    out[length(out)] <- paste0(out[length(out)], ",")
  }
  out
}

style_recursive_loop <- function(expr){
  args <- expr[-1]
  text <- character(0)
  for (i in seq_along(args)){
    recurse <- is_target_call(args[[i]]) || is_trigger_call(args[[i]])
    if (recurse){
      text <- c(
        text,
        style_recursive(
          expr = args[[i]],
          name = names(args)[i],
          append_comma = i < length(args)
        )
      )
    } else {
      text <- c(
        text,
        style_leaf(
          expr = args[[i]],
          name = names(args)[i],
          append_comma = i < length(args)
        )
      )
    }
  }
  text
}

style_leaf <- function(name, expr, append_comma){
  text <- styler::style_text(rlang::expr_text(expr))
  text[1] <- paste(name, "=", text[1])
  if (append_comma){
    text[length(text)] <- paste0(text[length(text)], ",")
  }
  text
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
