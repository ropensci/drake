decoded_path <- function(x) {
  substr(x, 2, nchar(x) - 1)
}

encoded_path <- function(x) {
  sprintf("\"%s\"", x)
}

redecode_path <- function(x) {
  substr(x, 2, nchar(x) - 1)
}

reencode_path <- function(x) {
  sprintf("\"%s\"", x)
}

display_path <- function(x) {
  x[is_encoded_path(x)] <- sprintf(
    "file %s",
    redecoded_path(x[is_encoded_path(x)], x)
  )
}

is_encoded_path <- function(x) {
  x <- substr(x = x, start = 0, stop = 1)
  x == "\"" | x == "'" # TODO: get rid of the single quote next major release
}

not_encoded_path <- function(x) {
  !is_encoded_path(x)
}
