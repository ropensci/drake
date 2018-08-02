#' @title Download and save the code and data files of an example
#'   `drake`-powered project.
#' @description The `drake_example()` function downloads a
#'   folder from <https://github.com/wlandau/drake-examples>.
#'   (Really, it downloads one of the zip files listed at
#'   <https://github.com/wlandau/drake-examples/tree/gh-pages>
#'   and unzips it. Do not include the `.zip` extension
#'   in the `example` argument.)
#' @seealso [drake_examples()], [make()]
#' @export
#' @return `NULL`
#' @param example name of the example.
#'   The possible values are the names of the folders at
#'   <https://github.com/wlandau/drake-examples>.
#' @param to Character scalar,
#'   the folder containing the code files for the example.
#'   passed to the `exdir` argument of `utils::unzip()`.
#' @param destination Deprecated, use `to` instead.
#' @param overwrite Logical, whether to overwrite an existing folder
#'   with the same name as the drake example.
#' @param quiet logical, passed to `downloader::download()`
#'   and thus `utils::download.file()`. Whether
#'   to download quietly or print progress.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' drake_examples() # List all the drake examples.
#' # Sets up the same example as https://ropenscilabs.github.io/drake-manual/mtcars.html # nolint
#' drake_example("mtcars")
#' # Sets up the SLURM example.
#' drake_example("slurm")
#' })
#' }
drake_example <- function(
  example = "main",
  to = getwd(),
  destination = NULL,
  overwrite = FALSE,
  quiet = TRUE
){
  assert_pkgs("downloader")
  if (!is.null(destination)){
    warning(
      "The 'destination' argument of drake_example() is deprecated. ",
      "Use 'to' instead."
    )
    to <- destination
  }
  url <- file.path(
    "https://wlandau.github.io/drake-examples",
    paste0(example, ".zip")
  )
  zip <- paste0(tempfile(), ".zip")
  downloader::download(url = url, destfile = zip, quiet = quiet)
  utils::unzip(zip, exdir = to, overwrite = overwrite)
  invisible()
}

#' @title List the names of all the drake examples.
#' @description You can find the code files of the examples at
#'   <https://github.com/wlandau/drake-examples>.
#'   The `drake_examples()` function downloads the list of examples
#'   from <https://wlandau.github.io/drake-examples/examples.md>,
#'   so you need an internet connection.
#' @export
#' @seealso [drake_example()], [make()]
#' @return Names of all the drake examples.
#' @param quiet logical, passed to `downloader::download()`
#'   and thus `utils::download.file()`. Whether
#'   to download quietly or print progress.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' drake_examples() # List all the drake examples.
#' # Sets up the example from
#' # https://ropenscilabs.github.io/drake-manual/mtcars.html
#' drake_example("mtcars")
#' # Sets up the SLURM example.
#' drake_example("slurm")
#' })
#' }
drake_examples <- function(quiet = TRUE) {
  assert_pkgs("downloader")
  destfile <- tempfile()
  downloader::download(
    url = "https://wlandau.github.io/drake-examples/examples.md",
    destfile = destfile,
    quiet = quiet
  )
  scan(destfile, what = character(1), quiet = TRUE) %>%
    fs::path_ext_remove()
}
