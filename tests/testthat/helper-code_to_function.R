setup_scripts <- function() {
  script1 <- tempfile()
  script2 <- tempfile()
  script3 <- tempfile()

  tempDir <- normalizePath(tempdir(),winslash = "/")
  writeLines(c("data <- mtcars",
               "data$newcol <- 1",
               paste0("saveRDS(data, \"",tempDir,"/testobject.rds\")")), script1)
  writeLines(c(paste0("data <- readRDS(\"",tempDir,"/testobject.rds\")"),
               "data$newcol2 <- 2",
               paste0("saveRDS(data, \"",tempDir,"/testobject2.rds\")")), script2)
  writeLines(c(paste0("data <- readRDS(\"",tempDir,"/testobject2.rds\")"),
               "plot(data$newcol,data$newcol1)"), script3)

  return(list(script1 = script1,
              script2 = script2,
              script3 = script3,
              tempDir = tempDir))
}

update_script2_trivial <- function(script,tempDir) {
  writeLines(c(paste0("data <- readRDS(\"",tempDir,"/testobject.rds\")"),
               "# trivial changes",
               "data$newcol2 <- 2",
               paste0("saveRDS(data, \"",tempDir,"/testobject2.rds\")")), script)
  code_to_function(script)
}

update_script2_non_trivial <- function(script,tempDir) {
  writeLines(c(paste0("data <- readRDS(\"",tempDir,"/testobject.rds\")"),
               "data$newcol2 <- 3",
               paste0("saveRDS(data, \"",tempDir,"/testobject2.rds\")")), script)
  code_to_function(script)
}

update_script2_same_output <- function(script,tempDir) {
  writeLines(c(paste0("data <- readRDS(\"",tempDir,"/testobject.rds\")"),
               "data$newcol2 <- 4",
               "data$newcol2 <- 3",
               paste0("saveRDS(data, \"",tempDir,"/testobject2.rds\")")), script)
  code_to_function(script)
}



create_scripts_plan <- function() {
  drake_plan(
    step1 = script1_function(),
    step2 = script2_function(step1),
    step3 = script3_function(step2)
  )
}
