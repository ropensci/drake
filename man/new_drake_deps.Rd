% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/s3_drake_deps.R
\name{new_drake_deps}
\alias{new_drake_deps}
\title{\code{drake_deps} constructor}
\usage{
new_drake_deps(
  globals = character(0),
  namespaced = character(0),
  strings = character(0),
  loadd = character(0),
  readd = character(0),
  file_in = character(0),
  file_out = character(0),
  knitr_in = character(0)
)
}
\arguments{
\item{globals}{Global symbols found in the expression}

\item{namespaced}{Namespaced objects, e.g. \code{rmarkdown::render}.}

\item{strings}{Miscellaneous strings.}

\item{loadd}{Targets selected with \code{\link[=loadd]{loadd()}}.}

\item{readd}{Targets selected with \code{\link[=readd]{readd()}}.}

\item{file_in}{Literal static file paths enclosed in \code{\link[=file_in]{file_in()}}.}

\item{file_out}{Literal static file paths enclosed in \code{\link[=file_out]{file_out()}}.}

\item{knitr_in}{Literal static file paths enclosed in \code{\link[=knitr_in]{knitr_in()}}.}
}
\value{
A \code{drake_deps} object.
}
\description{
List of class \code{drake_deps}.
}
\examples{
if (FALSE) { # stronger than roxygen dontrun
new_drake_deps()
}
}
\keyword{internal}
