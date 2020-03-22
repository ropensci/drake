# Taken from https://github.com/r-lib/lifecycle/blob/master/R/badge.R
# under GPL-3. Copyright RStudio.
lifecycle <- function(stage) {
  url <- paste0("https://www.tidyverse.org/lifecycle/#", stage)
  img <- lifecycle_img(stage, url)
  sprintf("\\ifelse{html}{%s}{\\strong{%s}}", img, upcase1(stage))
}

lifecycle_img <- function(stage, url) {
  file <- sprintf("lifecycle-%s.svg", stage)
  stage_alt <- upcase1(stage)
  switch(
    stage,
    # nolint start
    experimental = ,
    maturing = ,
    stable = ,
    questioning = ,
    retired = ,
    # nolint end
    archived = sprintf(
      "\\out{<a href='%s'><img src='%s' alt='%s lifecycle'></a>}",
      url,
      file.path("figures", file),
      stage_alt
    ),
    # nolint start
    `soft-deprecated` = ,
    deprecated = ,
    # nolint end
    defunct = sprintf(
      "\\figure{%s}{options: alt='%s lifecycle'}",
      file,
      stage_alt
    ),
    stop0(sprintf("Unknown lifecycle stage `%s`", stage))
  )
}

upcase1 <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}
