#' @title Function \code{clean}
#' @description Cleans up all work done by \code{\link{run}}. 
#' Your working directory (\code{\link{getwd}()}) must be the 
#' root directory of your drake project.
#' WARNING:
#' This deletes ALL \code{\link{run}} output, which includes 
#' file outputs as well as the entire drake cache. Only use \code{clean}
#' if you're sure you won't lose any important work.
#' @seealso \code{\link{prune}}, \code{\link{run}}, 
#' @export
#' @param destroy logical, whether to totally remove the drake cache. 
#' If \code{destroy} is \code{FALSE}, only the outputs from \code{run}()
#' are removed. If \code{TRUE}, the whole cache is removed, including
#' session metadata. 
clean = function(destroy = FALSE){
  if(!file.exists(cachepath)) return(invisible())
  cache = storr_rds(cachepath, mangle_key = TRUE)
  files = cached() %>% Filter(f = is_file) 
  remove_output_files(files, cache)
  if(destroy){
    unlink(cachepath, recursive = TRUE)
  } else {
    cache$clear()
    cache$clear(namespace = "depends")
  }
  invisible()
}

#' @title Function \code{prune}
#' @description Removes any cached output objects and generated 
#' files not listed in \code{plan$output$}. 
#' Your working directory (\code{\link{getwd}()}) must be the
#' root directory of your project.
#' WARNING: this removes files.
#' Only do this if you're sure you won't lose any important work.
#' @seealso \code{\link{clean}}, \code{\link{run}},
#' \code{\link{help_drake}}
#' @export
#' @param workflow data frame, workflow as generated with \code{\link{plan}}. 
prune = function(workflow){
  if(!file.exists(cachepath)) return(invisible())
  cache = storr_rds(cachepath, mangle_key = TRUE)
  remove = setdiff(cached(), workflow$target)
  files = Filter(remove, f = is_file)
  remove_output_files(files, cache)
  lapply(remove, function(x){
    cache$del(x)
    cache$del(x, namespace = "depends")
  })
  invisible()
}

remove_output_files = Vectorize(function(file, cache){
  if(!is_imported(file)) unlink(unquote(file), recursive = TRUE)
}, "file")
