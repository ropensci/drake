#' @title Function silencer_hook
#' @description an example \code{hook} argument to
#' \code{make()} that redirects output and error messages
#  to separate files.
#' @export
#' @seealso \code{\link{make}}, \code{\link{message_sink_hook}},
#' \code{\link{output_sink_hook}}
#' @param code code to run to build the target.
#' @examples \dontrun{
#' # Test out the silencer hook on its own.
#' silencer_hook({
#'   cat(1234)
#'   stop(5678)
#' })
#' # Make a new workflow plan.
#' x <- workplan(loud = cat(1234), bad = stop(5678))
#' # Test out the silencer hook on a drake project.
#' make(x, hook = silencer_hook) # All output should be suppressed.
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
#' # Test out the message sink hook on its own.
#' message_sink_hook({
#'   cat(1234)
#'   stop(5678)
#' })
#' # Create a new workflow plan.
#' x <- workplan(loud = cat(1234), bad = stop(5678))
#' # Run the project. All messages should be suppressed.
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
#' # Test out the output sink hook on its own.
#' output_sink_hook({
#'   cat(1234)
#'   stop(5678)
#' })
#' # Create a new workflow plan.
#' x <- workplan(loud = cat(1234), bad = stop(5678))
#' # Run the project. Standard output (via cat() and print())
#' # should be suppressed, but messages should persist.
#' make(x, hook = output_sink_hook)
#' }
output_sink_hook <- function(code){
  output <- paste0("output", Sys.getpid(), ".txt")
  on.exit(suppressWarnings(sink(type = "output")))
  sink(output, type = "output")
  force(code)
}

#' @title Function empty_hook
#' @description a \code{hook} argument to \code{\link{make}()}
#' for which no targets get built and no imports get resolved
#' @export
#' @param code Placeholder for the code to build a target/import.
#' For \code{empty_hook}, this code does not actually get executed.
#' @examples
#' \dontrun{
#' load_basic_example() # Load the canonical example for drake.
#' # Run the project with the empty hook.
#' make(my_plan, hook = empty_hook) # Nothing gets built!
#' cached() # character(0) # nolint
#' }
empty_hook <- function(code){
  invisible()
}

#' @title Function default_hook
#' @description The default \code{hook} argument to \code{\link{make}()}.
#' @export
#' @param code Placeholder for the code to build a target/import.
#' @examples
#' \dontrun{
#' load_basic_example() # Load the canonical drake example.
#' # Everything gets built normally.
#' make(my_plan, hook = default_hook)
#' cached() # List the cached targets and imports.
#' }
default_hook <- function(code){
  force(code)
}
