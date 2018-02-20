dir <- rprojroot::find_root(criterion = "DESCRIPTION", path = getwd())
dest <- file.path(dir, "docs")
if (!file.exists(dest)){
  dir.create(dest)
}
site_dir <- tempdir()
index_file <- file.path(site_dir, "index.html")

tmp <- pkgdown::build_site(pkg = dir, path = site_dir, preview = FALSE)
x <- readLines(index_file)
icon <- file.path(dir, "images", "icon.ico")
tmp <- file.copy(from = icon, to = site_dir, overwrite = TRUE)

from <- "<p><img src=.*images/graph.png.*></p>"
to <- "<iframe
  src = 'https://cdn.rawgit.com/ropensci/drake/e87f05ad/images/reg2.html'
  width = '100%' height = '600px' allowtransparency='true'
  style='border: none; box-shadow: none'>
  </iframe>"
x <- gsub(pattern = from, replacement = to, x = x)

from <- "<title>.*</title>"
to <- paste(
  "<title>drake</title>",
  "<link rel=\"drake icon\" type = \"image/x-icon\" href=\"icon.ico\"/>", # nolint
  collapse = "\n"
)
x <- gsub(pattern = from, replacement = to, x = x)

tmp <- writeLines(text = x, con = index_file)
from <- list.files(site_dir, full.names = TRUE)
tmp <- file.copy(from = from, to = dest, overwrite = TRUE, recursive = TRUE)
