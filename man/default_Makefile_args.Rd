% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{default_Makefile_args}
\alias{default_Makefile_args}
\title{Default arguments of Makefile parallelism
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}}
\usage{
default_Makefile_args(jobs, verbose)
}
\arguments{
\item{jobs}{Number of jobs.}

\item{verbose}{Integer, control printing to the console/terminal.
\itemize{
\item \code{0}: print nothing.
\item \code{1}: print target-by-target messages as \code{\link[=make]{make()}} progresses.
\item \code{2}: show a progress bar to track how many targets are
done so far.
}}
}
\value{
\code{args} for \code{system2(command, args)}
}
\description{
2019-01-03
}
\keyword{internal}
