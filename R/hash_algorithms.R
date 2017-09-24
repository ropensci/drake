#' @title Default short hash algorithm for \code{make()}
#' @export
#' @seealso \code{\link{make}}, \code{\link{available_hash_algos}}
#' @description Hashing is advanced. Most users
#' do not need to know about this function.
#' @details
#' The short algorithm must be among \code{\link{available_hash_algos}{}},
#' which is just the collection of algorithms available to the `algo`
#' argument in \code{digest::digest()}. \cr \cr
#'
#' If you express no preference for a hash, drake will use
#' the short hash for the existing project, or
#' \code{\link{default_short_hash_algo}()} for a new project.
#' If you do supply a hash algorithm, it will only apply to
#' fresh projects (see \code{\link{clean}(destroy = TRUE)}).
#' For a project that already exists, if you supply a hash algorithm,
#' drake will warn you and then ignore your choice, opting instead for
#' the hash algorithm already chosen for the project
#' in a previous \code{make()}. \cr \cr
#'
#' Drake uses both a short hash algorithm
#' and a long hash algorithm. The shorter hash has fewer characters,
#' and it is used to generate the names of internal cache files
#' and auxiliary files. The decision for short names is important
#' because Windows places restrictions on the length of file paths.
#' On the other hand, some internal hashes in drake are
#' never used as file names, and those hashes can use a longer hash
#' to avoid collisions.
#' @examples
#' default_short_hash_algo()
default_short_hash_algo <- function() {
  "xxhash64"
}

#' @title Default long hash algorithm for \code{make()}
#' @export
#' @seealso \code{\link{make}}, \code{\link{available_hash_algos}}
#' @description Hashing is advanced. Most users
#' do not need to know about this function.
#' @details
#' The long algorithm must be among \code{\link{available_hash_algos}{}},
#' which is just the collection of algorithms available to the `algo`
#' argument in \code{digest::digest()}. \cr \cr
#'
#' If you express no preference for a hash, drake will use
#' the long hash for the existing project, or
#' \code{\link{default_long_hash_algo}()} for a new project.
#' If you do supply a hash algorithm, it will only apply to
#' fresh projects (see \code{\link{clean}(destroy = TRUE)}).
#' For a project that already exists, if you supply a hash algorithm,
#' drake will warn you and then ignore your choice, opting instead for
#' the hash algorithm already chosen for the project
#' in a previous \code{make()}. \cr \cr
#'
#' Drake uses both a short hash algorithm
#' and a long hash algorithm. The shorter hash has fewer characters,
#' and it is used to generate the names of internal cache files
#' and auxiliary files. The decision for short names is important
#' because Windows places restrictions on the length of file paths.
#' On the other hand, some internal hashes in drake are
#' never used as file names, and those hashes can use a longer hash
#' to avoid collisions.
#' @examples
#' default_long_hash_algo()
default_long_hash_algo <- function() {
  "sha256"
}

#' @title Function available_hash_algos
#' @export
#' @description List the available hash algorithms.
#' @examples
#' available_hash_algos()
available_hash_algos <- function(){
  eval(formals(digest::digest)$algo)
}
