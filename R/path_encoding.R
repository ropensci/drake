decode_path <- function(x){
  substr(x, 2, nchar(x) - 1)
}

encode_path <- function(x){
  sprintf("\"%s\"", x)
}

is_encoded_path <- function(x) {
  x <- substr(x = x, start = 0, stop = 1)
  x == "\"" | x == "'" # TODO: get rid of the single quote next major release
}

not_encoded_path <- function(x) {
  !is_encoded_path(x)
}
