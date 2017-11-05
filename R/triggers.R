#' @title Function triggers
#' @export
#' @seealso \code{\link{workplan}}, \code{\link{make}}
#' @description List the available triggers.
#' @details By default, \code{make()}
#' builds targets that need updating and
#' skips over the ones that are already up to date.
#' In other words, a change in a dependency, workflow plan command,
#' or file, or the lack of the target itself,
#'\emph{triggers} the build process for the target.
#' You can relax this behavior by choosing a trigger for each target.
#' Set the trigger for each target with a \code{"trigger"}
#' column in your workflow plan data frame. The \code{triggers()}
#' function lists the available triggers:
#' 
#' \itemize{
#'   \item{'any'}{: 
#'     Build the target if any of the other triggers activate.
#'   }
#'
#'   \item{'missing'}{: 
#'     Build if the target itself is missing. Always applies.
#'   }
#'
#'   \item{'file'}{: 
#'     Build if the target is a file and
#'     that output file is either missing or corrupted.
#'     Also build if \code{missing} is triggered.
#'   }
#'
#'   \item{'command'}{: 
#'     Build if the workflow plan command has changed since last
#'     time the target was built. Also built if \code{missing} is triggered.
#'   }
#'
#'   \item{'depends'}{: 
#'     Build if any of the target's dependencies
#'     has changed since the last \code{\link{make}()}.
#'     Also build if \code{missing} is triggered.
#'   }
#' }
#' 
#' @examples
#' triggers()
#' \dontrun{
#' load_basic_example()
#' my_plan$trigger = "command"
#' # You can have different triggers for different targets.
#' my_plan$trigger[1] = "file"
#' make(my_plan)
#' # Change a dependency
#' function(d) {
#'   d$x3 <- d$x ^ 3
#'   lm(y ~ x3, data = d)
#' }
#' make(my_plan) # Nothing changes!
#' make(my_plan, rush = TRUE) # Same as the "missing" trigger everywhere.
#' }
triggers <- function(){
  c(
    "missing",
    "file",
    "command",
    "depends",
    "any"
  )
}

assert_legal_triggers <- function(x){
  x <- setdiff(x, triggers())
  if (!length(x)){
    return(invisible())
  }
  stop(
    "Illegal triggers found. See triggers() for the legal ones. Illegal:\n",
    multiline_message(x),
    call. = FALSE
  )
}

should_build <- function(target, meta_list, config){
  if (meta_list[[target]]$imported) {
    return(TRUE)
  }
  should_build_target(
    target = target,
    meta = meta_list[[target]],
    config = config
  )
}

should_build_target <- function(target, meta, config){
  if (!(target %in% config$inventory$kernels)){
    return(FALSE)
  }
  if (!file_current(target = target, meta = meta, config = config)){
    return(FALSE)
  }
  identical(
    config$cache$get(target, namespace = "commands"),
    meta$command
  ) &
  identical(
    config$cache$get(target, namespace = "depends"),
    meta$depends
  )
}

file_current <- function(target, meta, config){
  if (!is_file(target)){
    return(TRUE)
  }
  if (!file.exists(unquote(target))){
    return(FALSE)
  }
  tryCatch(
    identical(
      config$cache$get(target, namespace = "kernels"),
      meta$file
    ),
    error = error_false
  )
}

log_attempts <- Vectorize(function(targets, config){
  config$cache$set(key = targets, value = targets,
    namespace = "attempts")
  invisible()
},
"targets")
