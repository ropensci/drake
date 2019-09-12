context("test-code_to_function")

test_that("contents of functions are identical to original script", {

  script1 <- tempfile()
  writeLines(c("data <- mtcars", "data$newcol<-1","data"), script1)

  script1_function<-code_to_function(script1)

  function_contents<-body(script1_function)

  expect_equal(function_contents,
               c("{",readLines(script1)))
})

test_that("Functions are seen as equivalent in drake", {

  script1 <- tempfile()
  script2 <- tempfile()
  writeLines(c("data <- mtcars", "data$newcol<-1","data"), script1)
  writeLines(c("plot(data)"), script2)

  function_1<-script_function1<-code_to_function(script1)
  function_2<-script_function2<-code_to_function(script2)

  script_plan<-drake_plan(
    dataset1 = function_1(),
    plot     = function_2(dataset1)
  )
  script_config <- drake_config(script_plan)


  function_1<-function(...){
    data<-mtcars
    data$newcol<-1
    data
  }
  function_2<-function(...){
    plot(data)
  }
  functional_plan<-drake_plan(
    dataset1 = function_1(),
    plot     = function_2(dataset1)
  )
  functional_config <- drake_config(functional_plan)

  expect_equal(function_1,script_function1)
  expect_equal(function_2,script_function2)
  expect_equal(functional_plan,script_plan)
  expect_equal(functional_config,script_config)

})

