unlink("docs", recursive = TRUE)
pkgdown::build_site()
if (!file.exists("docs")) {
  dir.create("docs")
}
file.copy("images", "docs", recursive = TRUE)
