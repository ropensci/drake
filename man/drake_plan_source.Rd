% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drake_plan_helpers.R
\name{drake_plan_source}
\alias{drake_plan_source}
\title{Show the code required to produce a given \code{drake} plan
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}}
\usage{
drake_plan_source(plan)
}
\arguments{
\item{plan}{A workflow plan data frame (see \code{\link[=drake_plan]{drake_plan()}})}
}
\value{
a character vector of lines of text. This text
is a call to \code{\link[=drake_plan]{drake_plan()}} that produces the plan you provide.
}
\description{
You supply a plan, and \code{\link[=drake_plan_source]{drake_plan_source()}}
supplies code to generate that plan. If you have the
\code{prettycode} package,
installed, you also get nice syntax highlighting in the console
when you print it.
}
\examples{
plan <- drake::drake_plan(
  small_data = download_data("https://some_website.com"),
  large_data_raw = target(
    command = download_data("https://lots_of_data.com"),
    trigger = trigger(
      change = time_last_modified("https://lots_of_data.com"),
      command = FALSE,
      depend = FALSE
    ),
    timeout = 1e3
  )
)
print(plan)
if (requireNamespace("styler", quietly = TRUE)) {
  source <- drake_plan_source(plan)
  print(source) # Install the prettycode package for syntax highlighting.
  file <- tempfile() # Path to an R script to contain the drake_plan() call.
  writeLines(source, file) # Save the code to an R script.
}
}
\seealso{
\code{\link[=drake_plan]{drake_plan()}}
}
