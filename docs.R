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
  site_dir <- file.path(dir, "docs")
  if (!file.exists(site_dir)){
    dir.create(site_dir)
  }
  index_file <- file.path(site_dir, "index.html")
  tmp <- pkgdown::build_site(pkg = dir, preview = FALSE)

  x <- readLines(index_file)
  from <- '<p><a href="https://ropensci.github.io/drake/images/reg2.html"><img src="./docs/images/graph.png"></a></p>' # nolint
  to <- "<iframe
    src = 'https://ropensci.github.io/drake/images/reg2.html'
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
  unlink(
    c(
      file.path(site_dir, "*.rds"),
      file.path(site_dir, "file*"),
      file.path(site_dir, "preview-")
    ),
    recursive = TRUE
  )
}

# Generate the HTML widgets in the docs/images/ folder.
# These interactive graphs are embedded in the vignettes.
# Requires pandoc.
images <- function(){
  library(here)
  html_out <- function(...) here::here("docs", "images", ...)
  for (dir in c(here::here("docs"), here::here("docs", "images"))){
    if (!file.exists(dir)){
      dir.create(dir)
    }
  }
  for (img in list.files(here::here("images"))){
    file.copy(
      here::here("images", img),
      here::here("docs", "images", img),
      overwrite = TRUE
    )
  }
  file.copy(
    here::here("docs", "images", "icon.ico"),
    here::here("docs", "icon.ico"),
    overwrite = TRUE
  )
  devtools::load_all() # load current drake
  clean(destroy = TRUE)
  config <- load_basic_example(overwrite = TRUE)
  vis_drake_graph(
    config, file = html_out("outdated.html"), selfcontained = TRUE,
                  width = "100%", height = "500px")
  config <- make(my_plan)
  vis_drake_graph(config, file = html_out("built.html"), selfcontained = TRUE,
                  width = "100%", height = "500px")
  reg2 <- function(d){
    d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d)
  }
  vis_drake_graph(config, file = html_out("reg2.html"), selfcontained = TRUE,
                  width = "100%", height = "500px")
  vis_drake_graph(
    config, file = html_out("reg2-small-legend.html"), selfcontained = TRUE,
                  width = "100%", height = "500px", full_legend = FALSE)
  vis_drake_graph(
    config, file = html_out("reg2-no-legend.html"), selfcontained = TRUE,
                  width = "100%", height = "500px", ncol_legend = 0)
  vis_drake_graph(
    config, file = html_out("targetsonly.html"), selfcontained = TRUE,
    targets_only = TRUE,
    width = "100%", height = "500px",
    from = c("large", "small")
  )
  vis_drake_graph(
    config, file = html_out("fromout.html"), selfcontained = TRUE,
    width = "100%", height = "500px",
    from = c("regression2_small", "regression2_large")
  )
  vis_drake_graph(
    config, file = html_out("fromin.html"), selfcontained = TRUE,
    width = "100%", height = "500px",
    from = "small", mode = "in"
  )
  vis_drake_graph(
    config, file = html_out("fromall.html"), selfcontained = TRUE,
    width = "100%", height = "500px",
    from = "small", mode = "all", order = 1
  )
  vis_drake_graph(
    config, file = html_out("subset.html"), selfcontained = TRUE,
    width = "100%", height = "500px",
    subset = c("regression2_small", "\"report.md\"")
  )
  clean(destroy = TRUE)
  unlink("report.Rmd")

  # For the "packages" example.
  rm(config)
  library(magrittr)
  reportfile <- file.path("examples", "packages", "report.Rmd") %>%
    system.file(package = "drake", mustWork = TRUE)
  file.copy(reportfile, getwd())
  runfile <- file.path("examples", "packages", "interactive-tutorial.R") %>%
    system.file(package = "drake", mustWork = TRUE)
  source(runfile)
  vis_drake_graph(
    config, file = html_out("packages.html"), selfcontained = TRUE,
    width = "100%", height = "500px"
  )

  # For the best practices vignette
  get_data <- function(){
    "Get the data."
  }
  analyze_data <- function(){
    "Analyze the data."
  }
  summarize_results <- function(){
    "Summarize the results."
  }
  files <- c("data.csv", "get_data.R", "analyze_data.R", "summarize_data.R")
  for (file in files){
    file.create(file)
  }
  my_plan <- drake_plan(
    my_data = get_data(file_in("data.csv")),
    my_analysis = analyze_data(my_data),
    my_summaries = summarize_results(my_data, my_analysis),
    strings_in_dots = "literals"
  )
  config <- drake_config(my_plan)
  vis_drake_graph(
    main = "Good workflow plan",
    config, file = html_out("good-commands.html"), selfcontained = TRUE,
    width = "100%", height = "500px"
  )
  my_plan <- drake_plan(
    my_data = source(file_in("get_data.R")),
    my_analysis = source(file_in("analyze_data.R")),
    my_summaries = source(file_in("summarize_data.R")),
    strings_in_dots = "literals"
  )
  config <- drake_config(my_plan)
  vis_drake_graph(
    main = "Bad workflow plan",
    config, file = html_out("bad-commands.html"), selfcontained = TRUE,
    width = "100%", height = "500px"
  )

  # Clean up.
  for (file in files){
    file.remove(file)
  }
  clean(destroy = TRUE)
  unlink(c("figure", "report.Rmd"), recursive = TRUE)
  unlink(html_out("*_files"), recursive = TRUE)
}

faq()
pkgdown()
images()
