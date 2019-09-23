setup_scripts <- function() {
  script1 <- tempfile()
  script2 <- tempfile()
  script3 <- tempfile()

  tempDir <- normalizePath(tempdir(), winslash = "/")
  writeLines(c("data <- mtcars",
               "data$newcol <- 1",
               paste0("saveRDS(data, \"",
                      file.path(tempDir, "testobject.rds"), "\")")),
             script1)
  writeLines(c(paste0("data <- readRDS(\"",
                      file.path(tempDir, "testobject.rds"), "\")"),
               "data$newcol2 <- 2",
               paste0("saveRDS(data, \"",
                      file.path(tempDir, "testobject2.rds"), "\")")),
             script2)
  writeLines(c(paste0("data <- readRDS(\"",
                      file.path(tempDir, "testobject2.rds"), "\")"),
               "plot(data$newcol,data$newcol2)"),
             script3)

  return(list(script1 = script1,
              script2 = script2,
              script3 = script3,
              tempDir = tempDir))
}

update_script2_trivial <- function(script, tempDir) {
  writeLines(c(paste0("data <- readRDS(\"",
                      file.path(tempDir, "testobject.rds"), "\")"),
               "# trivial changes",
               "data$newcol2 <- 2",
               paste0("saveRDS(data, \"",
                      file.path(tempDir, "testobject2.rds"), "\")")),
             script)
  code_to_function(script)
}

update_script2_non_trivial <- function(script, tempDir) {
  writeLines(c(paste0("data <- readRDS(\"",
                      file.path(tempDir, "testobject.rds"), "\")"),
               "data$newcol2 <- 3",
               paste0("saveRDS(data, \"",
                      file.path(tempDir, "testobject2.rds"), "\")")),
             script)
  code_to_function(script)
}

update_script2_same_output <- function(script, tempDir) {
  writeLines(c(paste0("data <- readRDS(\"",
                      file.path(tempDir, "testobject.rds"), "\")"),
               "data$newcol2 <- 4",
               "data$newcol2 <- 3",
               paste0("saveRDS(data, \"",
                      file.path(tempDir, "testobject2.rds"), "\")")),
             script)
  code_to_function(script)
}



create_scripts_plan <- function() {
  drake_plan(
    step1 = script1_function(),
    step2 = script2_function(step1),
    step3 = script3_function(step2)
  )
}

setup_rmd <- function() {
  rmd1 <- tempfile()
  rmd2 <- tempfile()
  rmd3 <- tempfile()

  tempDir <- normalizePath(tempdir(), winslash = "/")
  writeLines(c(
    "---",
    "title: \"Test RMD 1\"",
    "author: \"Unknown\"",
    "output: html_document",
    "---",
    "This is a test rmd to load and save the `mtcars` dataset.",
    "```{r load_data}",
    "data <- mtcars",
    "```",
    "We will now add a new column!",
    "```{r manipulate}",
    "data$newcol <- 1",
    "```",
    "Now, we will save it!",
    "```{r save_data}",
    paste0("saveRDS(data, \"", file.path(tempDir, "testobject.rds"), "\")"),
    "```"),
    rmd1)
  writeLines(c(
      "---",
      "title: \"Test RMD 2\"",
      "author: \"Unknown\"",
      "output: html_document",
      "---",
      "This is a test rmd to load a priorly edited `mtcars` dataset.",
      "```{r load_data}",
      paste0("data <- readRDS(\"", file.path(tempDir, "testobject.rds"), "\")"),
      "```",
      "We will now add a new column!",
      "```{r manipulate}",
      "data$newcol2 <- 2",
      "```",
      "Now, we will save it!",
      "```{r save_data}",
      paste0("saveRDS(data, \"", file.path(tempDir, "testobject2.rds"), "\")"),
      "```"),
      rmd2)
  writeLines(c(
    "---",
    "title: \"Test RMD 3\"",
    "author: \"Unknown\"",
    "output: html_document",
    "---",
    "This is a test rmd to load and plot a priorly edited `mtcars` dataset.",
    "```{r load_data}",
    paste0("data <- readRDS(\"", file.path(tempDir, "testobject2.rds"), "\")"),
    "```",
    "We will now plot our data!",
    "```{r plot}",
    "plot(data$newcol,data$newcol2)",
    "```"),
    rmd3)

  return(list(rmd1 = rmd1,
              rmd2 = rmd2,
              rmd3 = rmd3,
              tempDir = tempDir))
}

update_rmd2_trivial <- function(rmd, tempDir) {
  writeLines(c(
    "---",
    "title: \"Test RMD 2\"",
    "author: \"Unknown\"",
    "output: html_document",
    "---",
    "This is a test rmd to load a priorly edited `mtcars` dataset.",
    "```{r load_data}",
    paste0("data <- readRDS(\"", file.path(tempDir, "testobject.rds"), "\")"),
    "```",
    "We will now add a new column! I am adding a trivial change - a comment.",
    "```{r manipulate}",
    "# trivial changes",
    "data$newcol2 <- 2",
    "```",
    "Now, we will save it!",
    "```{r save_data}",
    paste0("saveRDS(data, \"", file.path(tempDir, "testobject2.rds"), "\")"),
    "```"),
    rmd)
  code_to_function(rmd)
}

update_rmd2_non_trivial <- function(rmd, tempDir) {
  writeLines(c(
    "---",
    "title: \"Test RMD 2\"",
    "author: \"Unknown\"",
    "output: html_document",
    "---",
    "This is a test rmd to load a priorly edited `mtcars` dataset.",
    "```{r load_data}",
    paste0("data <- readRDS(\"", file.path(tempDir, "testobject.rds"), "\")"),
    "```",
    "We will now add a new column! I am adding a non trivial change -
    a different number.",
    "```{r manipulate}",
    "data$newcol2 <- 3",
    "```",
    "Now, we will save it!",
    "```{r save_data}",
    paste0("saveRDS(data, \"", file.path(tempDir, "testobject2.rds"), "\")"),
    "```"),
    rmd)
  code_to_function(rmd)
}

update_rmd2_same_output <- function(rmd, tempDir) {
  writeLines(c(
    "---",
    "title: \"Test RMD 2\"",
    "author: \"Unknown\"",
    "output: html_document",
    "---",
    "This is a test rmd to load a priorly edited `mtcars` dataset.",
    "```{r load_data}",
    paste0("data <- readRDS(\"", file.path(tempDir, "testobject.rds"), "\")"),
    "```",
    "We will now add a new column! I am adding a non trivial change -
    two manipulations that end up with the same output as before.",
    "```{r manipulate}",
    "data$newcol2 <- 4",
    "data$newcol2 <- 3",
    "```",
    "Now, we will save it!",
    "```{r save_data}",
    paste0("saveRDS(data, \"", file.path(tempDir, "testobject2.rds"), "\")"),
    "```"),
    rmd)
  code_to_function(rmd)

}



create_rmd_plan <- function() {
  drake_plan(
    step1 = rmd1_function(),
    step2 = rmd2_function(step1),
    step3 = rmd3_function(step2)
  )
}
