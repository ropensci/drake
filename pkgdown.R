dir <- rprojroot::find_root(criterion = "DESCRIPTION", path = getwd())
pkgdown::build_site(dir)
site_dir <- file.path(dir, "docs")
index_file <- file.path(site_dir, "index.html")
x <- readLines(index_file)
icon <- file.path(dir, "images", "icon.ico")
file.copy(from = icon, to = site_dir, overwrite = TRUE)

from <- "<p><img src=.*images/graph.png.*></p>"
to <- "<iframe
  src = 'https://cdn.rawgit.com/wlandau-lilly/drake/bd8a086f/images/reg2.html'
  width = '100%' height = '600px' allowtransparency='true'
  style='border: none; box-shadow: none'>
  </iframe>"
x <- gsub(pattern = from, replacement = to, x = x)

from <- "<title>.*</title>"
to <- paste(
  "<title>drake</title>",
  "<link rel=\"drake icon\" type = \"image/x-icon\" href=\"icon.ico\"/>",
  collapse = "\n"
)
x <- gsub(pattern = from, replacement = to, x = x)

writeLines(text = x, con = index_file)
