#' @title Function \command{clean}
#' @description Cleans up all work done by \command{\link{run}}. 
#' Your working directory (\command{\link{getwd}()}) must be the 
#' root directory of your drake project.
#' WARNING:
#' This deletes ALL \command{\link{run}} target, which includes 
#' file targets as well as the entire drake cache. Only use \command{clean}
#' if you're sure you won't lose any important work.
#' @seealso \command{\link{prune}}, \command{\link{run}}, 
#' @export
#' @param destroy logical, whether to totally remove the drake cache. 
#' If \command{destroy} is \command{FALSE}, only the targets from \command{run}()
#' are removed. If \command{TRUE}, the whole cache is removed, including
#' session metadata. 
clean = function(destroy = FALSE){
  if(!file.exists(cachepath)) return(invisible())
  cache = storr_rds(cachepath, mangle_key = TRUE)
  files = cached() %>% Filter(f = is_file) 
  remove_target_files(files, cache)
  if(destroy){
    unlink(cachepath, recursive = TRUE)
  } else {
    cache$clear()
    cache$clear(namespace = "depends")
  }
  invisible()
}

#' @title Function \command{prune}
#' @description Removes any cached target objects and generated 
#' files not listed in \command{plan$target$}. 
#' Your working directory (\command{\link{getwd}()}) must be the
#' root directory of your project.
#' WARNING: this removes files.
#' Only do this if you're sure you won't lose any important work.
#' @seealso \command{\link{clean}}, \command{\link{run}}
#' @export
#' @param plan data frame, plan as generated with \command{\link{plan}}. 
prune = function(plan){
  if(!file.exists(cachepath)) return(invisible())
  cache = storr_rds(cachepath, mangle_key = TRUE)
  remove = setdiff(cached(), plan$target)
  files = Filter(remove, f = is_file)
  remove_target_files(files, cache)
  lapply(remove, function(x){
    cache$del(x)
    cache$del(x, namespace = "depends")
  })
  invisible()
}

remove_target_files = Vectorize(function(file, cache){
  if(!is_imported(file)) unlink(unquote(file), recursive = TRUE)
}, "file")
