decoded_path <- function(x, config) {
  out <- lapply(
    X = x,
    FUN = function(x) {
      config$decode[[x]]
    }
  )
  unlist(out)
}

displayed_path <- function(x, config) {
  index <- is_encoded_path(x)
  pretty <- sprintf("file %s", decoded_path(x[index], config))
  c(x[!index], pretty)
}

redecode_path <- function(x) {
  substr(x, 2, nchar(x) - 1)
}

reencode_path <- function(x) {
  sprintf("\"%s\"", x)
}

redisplay_path <- function(x) {
  index <- is_encoded_path(x)
  pretty <- sprintf("file %s", redecode_path(x[index]))
  c(x[!index], pretty)
}

is_encoded_path <- function(x) {
  x <- substr(x = x, start = 0, stop = 1)
  x == "\"" | x == "'" # TODO: get rid of the single quote next major release
}

not_encoded_path <- function(x) {
  !is_encoded_path(x)
}
