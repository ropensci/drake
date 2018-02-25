faq <- function(){
  library(tidyverse)
  library(gh)

  is_faq <- function(label){
    identical(label$name, "frequently asked question")
  }

  any_faq_label <- function(issue){
    any(vapply(issue$labels, is_faq, logical(1)))
  }

  faq <- gh(
    "GET /repos/ropensci/drake/issues?state=all",
    .limit = Inf
  ) %>%
    Filter(f = any_faq_label)

  combine_fields <- function(lst, field){
    map_chr(lst, function(x){
      x[[field]]
    })
  }

  titles <- combine_fields(faq, "title")
  urls <- combine_fields(faq, "html_url")
  links <- paste0("- [", titles, "](", urls, ")")

  starter <- system.file(
    file.path("stubs", "faq.Rmd"),
    package = "drake",
    mustWork = TRUE
  )
  dir <- rprojroot::find_root(criterion = "DESCRIPTION", path = getwd())
  dest <- file.path(dir, "vignettes", "faq.Rmd")
  tmp <- file.copy(
    from = starter,
    to = dest,
    overwrite = TRUE
  )

  con <- file(dest, "a")
  writeLines(c("", links), con)
  close(con)
}

pkgdown <- function(){
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
    src = 'https://cdn.rawgit.com/ropensci/drake/ddefa828/images/reg2.html'
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
  unlink(
    c(
      file.path(dest, "*.rds"),
      file.path(dest, "file*"),
      file.path(dest, "preview-")
    ),
    recursive = TRUE
  )
}

faq()
pkgdown()
