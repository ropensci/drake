#' @title List the available drake triggers.
#' @export
#' @seealso [drake_plan()], [make()]
#' @description Triggers are target-level rules
#' that tell [make()] how to know if a target
#' is outdated or up to date.
#' @return A character vector with the names of the available triggers.
#' @details By default, `make()`
#' builds targets that need updating and
#' skips over the ones that are already up to date.
#' In other words, a change in a dependency, workflow plan command,
#' or file, or the lack of the target itself,
#'*triggers* the build process for the target.
#' You can relax this behavior by choosing a trigger for each target.
#' Set the trigger for each target with a `"trigger"`
#' column in your workflow plan data frame. The `triggers()`
#' function lists the available triggers:
#'
#' \itemize{
#'   \item{'any'}{:
#'     Build the target if any of the other triggers activate (default).
#'   }
#'
#'   \item{'command'}{:
#'     Build if the workflow plan command has changed since last
#'     time the target was built. Also built if `missing` is triggered.
#'   }
#'
#'   \item{'depends'}{:
#'     Build if any of the target's dependencies
#'     has changed since the last [make()].
#'     Also build if `missing` is triggered.
#'   }
#'
#'   \item{'file'}{:
#'     Build if the target is a file and
#'     that output file is either missing or corrupted.
#'     Also build if `missing` is triggered.
#'   }
#'
#'   \item{'missing'}{:
#'     Build if the target itself is missing. Always applies.
#'   }
#' }
#'
#' @examples
#' triggers()
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' load_mtcars_example() # Load drake's canonical example.
#' my_plan[["trigger"]] <- "command"
#' # You can have different triggers for different targets.
#' my_plan[["trigger"]][1] <- "file"
#' make(my_plan) # Run the project, build the targets.
#' # Change an imported dependency function.
#' reg2 <- function(d) {
#'   d$x3 <- d$x ^ 3
#'   lm(y ~ x3, data = d)
#' }
#' # Nothing changes! To react to `reg2`, you would need the
#' # "any" or "depends" trigger.
#' make(my_plan)
#' # You can use a global trigger if your workflow plan
#' # does not have a 'trigger' column.
#' my_plan[["trigger"]] <- NULL # Would override the global trigger.
#' make(my_plan, trigger = "missing") # Just build missing targets.
#' })
#' }
triggers <- function(){
  c(
    "any",
    "always",
    "command",
    "depends",
    "file",
    "missing"
  ) %>%
    sort
}

#' @title Return the default trigger.
#' @description Triggers are target-level rules
#' that tell [make()] how to check if
#' target is up to date or outdated.
#' @export
#' @seealso [triggers()], [make()]
#' @return A character scalar naming the default trigger.
#' @examples
#' default_trigger()
#' # See ?triggers for more examples.
default_trigger <- function(){
  "any"
}

triggers_with_command <- function(){
  c("any", "command")
}

triggers_with_depends <- function(){
  c("any", "depends")
}

triggers_with_file <- function(){
  c("any", "file")
}

get_trigger <- function(target, config){
  plan <- config$plan
  if (!(target %in% plan$target)){
    return("any")
  }
  drake_plan_override(
    target = target,
    field = "trigger",
    config = config
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

command_trigger <- function(target, meta, config){
  if (is.null(meta$command)){
    return(FALSE)
  }
  command <- get_from_subspace(
    key = target,
    subspace = "command",
    namespace = "meta",
    cache = config$cache
  )
  !identical(command, meta$command)
}

depends_trigger <- function(target, meta, config){
  if (is.null(meta$dependency_hash)){
    return(FALSE)
  }
  dependency_hash <- get_from_subspace(
    key = target,
    subspace = "dependency_hash",
    namespace = "meta",
    cache = config$cache
  )
  !identical(dependency_hash, meta$dependency_hash)
}

# We really need a file dependency hash just for all the files.
# Maybe that's the role of meta$file
file_trigger <- function(target, meta, config){
  for (file in meta$output_files){
    if (!file.exists(drake_unquote(file))){
      return(TRUE)
    }
  }
  for (hash_name in c("input_file_hash", "output_file_hash")){
    old_file_hash <- get_from_subspace(
      key = target,
      subspace = hash_name,
      namespace = "meta",
      cache = config$cache
    )
    if (!identical(old_file_hash, meta[[hash_name]])){
      return(TRUE)
    }
  }
  FALSE
}

should_build_target <- function(target, meta = NULL, config){
  if (is.null(meta)){
    meta <- drake_meta(target = target, config = config)
  }
  if (meta$imported) {
    return(TRUE)
  }
  if (meta$missing){
    return(TRUE)
  }
  trigger <- get_trigger(target = target, config = config)
  if (trigger == "always"){
    return(TRUE)
  }
  if (trigger %in% triggers_with_command()){
    if (command_trigger(target = target, meta = meta, config = config)){
      return(TRUE)
    }
  }
  if (trigger %in% triggers_with_depends()){
    if (depends_trigger(target = target, meta = meta, config = config)){
      return(TRUE)
    }
  }
  if (trigger %in% triggers_with_file()){
    if (file_trigger(target = target, meta = meta, config = config)){
      return(TRUE)
    }
  }
  FALSE
}

using_default_triggers <- function(config){
  default_plan_triggers <-
    is.null(config$plan[["trigger"]]) ||
    all(config$plan[["trigger"]] == default_trigger())
  default_plan_triggers && config$trigger == default_trigger()
}
