unlink("docs", recursive = TRUE)
pkgdown::build_site()
fs::dir_copy("images", "docs")
