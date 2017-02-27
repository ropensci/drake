#' @title Function \code{clean}
#' @description Cleans up all work done by \code{\link{run}}. 
#' Your working directory (\code{\link{getwd}()}) must be the 
#' root directory of your drake project.
#' WARNING:
#' This deletes ALL \code{\link{run}} target, which includes 
#' file targets as well as the entire drake cache. Only use \code{clean}
#' if you're sure you won't lose any important work.
#' @seealso \code{\link{prune}}, \code{\link{run}}, 
#' @export
#' @param destroy logical, whether to totally remove the drake cache. 
#' If \code{destroy} is \code{FALSE}, only the targets 
#' from \code{run}()
#' are removed. If \code{TRUE}, the whole cache is removed, including
#' session metadata. 
clean = function(destroy = FALSE){
  if(!file.exists(cachepath)) return(invisible())
  cache = storr_rds(cachepath, mangle_key = TRUE)
  files = cached() %>% Filter(f = is_file) 
  remove_target_files(files, cache)
  if(destroy) unlink(cachepath, recursive = TRUE)
  else uncache(cached())
  invisible()
}

#' @title Function \code{prune}
#' @description Removes any cached target objects and generated 
#' files not listed in \code{plan$target$}. 
#' Your working directory (\code{\link{getwd}()}) must be the
#' root directory of your project.
#' WARNING: this removes files.
#' Only do this if you're sure you won't lose any important work.
#' @seealso \code{\link{clean}}, \code{\link{run}}
#' @export
#' @param plan data frame, plan as generated with \code{\link{plan}}. 
prune = function(plan){
  if(!file.exists(cachepath)) return(invisible())
  cache = storr_rds(cachepath, mangle_key = TRUE)
  remove = setdiff(cached(), plan$target)
  files = Filter(remove, f = is_file)
  remove_target_files(files, cache)
  uncache(remove)
  invisible()
}

remove_target_files = Vectorize(function(file, cache){
  if(!is_imported(file)) unlink(unquote(file), recursive = TRUE)
}, "file")
