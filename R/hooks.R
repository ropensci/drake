#' @title Function silencer_hook
#' @description an example \code{hook} argument to
#' \code{make()} that redirects output and error messages
#  to separate files.
#' @export
#' @seealso \code{\link{make}}, \code{\link{message_sink_hook}},
#' \code{\link{output_sink_hook}}
#' @param code code to run to build the target.
#' @examples \dontrun{
#' x <- workplan(bad = stop())
#' make(x, hook = silencer_hook)
#' }
silencer_hook <- function(code){
  output_sink_hook(
    message_sink_hook(
      force(code)
    )
  )
}

#' @title Function message_sink_hook
#' @description an example \code{hook} argument to
#' \code{make()} that redirects error messages to files.
#' @export
#' @seealso \code{\link{make}}, \code{\link{silencer_hook}},
#' \code{\link{output_sink_hook}}
#' @param code code to run to build the target.
#' @examples \dontrun{
#' x <- workplan(bad = stop())
#' make(x, hook = message_sink_hook)
#' }
message_sink_hook <- function(code){
  message <- file(paste0("message", Sys.getpid(), ".txt"), "w")
  on.exit({
    suppressWarnings(sink(type = "message"))
    close(message)
  })
  sink(message, type = "message")
  force(code)
}

#' @title Function output_sink_hook
#' @description an example \code{hook} argument to
#' \code{make()} that redirects output messages to files.
#' @export
#' @seealso \code{\link{make}}, \code{\link{silencer_hook}},
#' \code{\link{message_sink_hook}}
#' @param code code to run to build the target.
#' @examples \dontrun{
#' x <- workplan(bad = stop())
#' make(x, hook = output_sink_hook)
#' }
output_sink_hook <- function(code){
  output <- paste0("output", Sys.getpid(), ".txt")
  on.exit(suppressWarnings(sink(type = "output")))
  sink(output, type = "output")
  force(code)
}
