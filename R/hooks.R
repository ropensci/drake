#' @title An example \code{hook} argument to
#' \code{make()} that redirects output and error messages
#  to separate files.
#' @description Most users do not need to micromanage hooks.
#' @export
#' @seealso \code{\link{make}}, \code{\link{message_sink_hook}},
#' \code{\link{output_sink_hook}}
#' @return A function that you can supply to the \code{hook} argument
#' of \code{\link{make}()}.
#' @param code code to run to build the target.
#' @examples \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # Test out the silencer hook on its own.
#' try(
#'   silencer_hook({
#'     cat(1234)
#'     stop(5678)
#'   }),
#'   silent = FALSE
#' )
#' # Make a new workflow plan.
#' x <- drake_plan(loud = cat(1234), bad = stop(5678))
#' # Test out the silencer hook on a drake project.
#' # All output should be suppressed.
#' try(make(x, hook = silencer_hook), silent = FALSE)
#' })
#' }
silencer_hook <- function(code){
  output_sink_hook(
    message_sink_hook(
      force(code)
    )
  )
}

#' @title An example \code{hook} argument to
#' \code{make()} that redirects error messages to files.
#' @description Most users do not need to micromanage hooks.
#' @export
#' @seealso \code{\link{make}}, \code{\link{silencer_hook}},
#' \code{\link{output_sink_hook}}
#' @return A function that you can supply to the \code{hook} argument
#' of \code{\link{make}()}.
#' @param code code to run to build the target.
#' @examples \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # Test out the message sink hook on its own.
#' try(
#'   message_sink_hook({
#'     cat(1234)
#'     stop(5678)
#'   }),
#'   silent = FALSE
#' )
#' # Create a new workflow plan.
#' x <- drake_plan(loud = cat(1234), bad = stop(5678))
#' # Run the project. All messages should be suppressed.
#' try(make(x, hook = message_sink_hook), silent = FALSE)
#' })
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

#' @title An example \code{hook} argument to
#' \code{make()} that redirects output messages to files.
#' @description Most users do not need to micromanage hooks.
#' @export
#' @seealso \code{\link{make}}, \code{\link{silencer_hook}},
#' \code{\link{message_sink_hook}}
#' @return A function that you can supply to the \code{hook} argument
#' of \code{\link{make}()}.
#' @param code code to run to build the target.
#' @examples \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # Test out the output sink hook on its own.
#' try(
#'   output_sink_hook({
#'     cat(1234)
#'     stop(5678)
#'   }),
#'   silent = FALSE
#' )
#' # Create a new workflow plan.
#' x <- drake_plan(loud = cat(1234), bad = stop(5678))
#' # Run the project. Standard output (via cat() and print())
#' # should be suppressed, but messages should persist.
#' try(make(x, hook = output_sink_hook), silent = FALSE)
#' })
#' }
output_sink_hook <- function(code){
  output <- paste0("output", Sys.getpid(), ".txt")
  on.exit(suppressWarnings(sink(type = "output")))
  sink(output, type = "output")
  force(code)
}

#' @title A \code{hook} argument to \code{\link{make}()}
#' for which no targets get built and no imports get processed.
#' @description This hook forces \code{\link{make}()}
#' to essentially do nothing.
#' @export
#' @return A function that you can supply to the \code{hook} argument
#' of \code{\link{make}()}.
#' @param code Placeholder for the code to build a target/import.
#' For \code{empty_hook}, this code does not actually get executed.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' # Run the project with the empty hook.
#' make(my_plan, hook = empty_hook) # Nothing gets built!
#' cached() # character(0) # nolint
#' })
#' }
empty_hook <- function(code){
  invisible()
}

#' @title Default \code{hook} argument to \code{\link{make}()}.
#' @description Most users do not need to micromanage hooks.
#' @export
#' @return A function that you can supply to the \code{hook} argument
#' of \code{\link{make}()}.
#' @param code Placeholder for the code to build a target/import.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_basic_example() # Get the code with drake_example("basic").
#' # Everything gets built normally.
#' make(my_plan, hook = default_hook)
#' cached() # List the cached targets and imports.
#' })
#' }
default_hook <- function(code){
  force(code)
}
