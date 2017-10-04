cat(get_testing_scenario_name(), ": ", sep = "")
context("knitr")

test_with_dir("deps_in_document() and dknit() work", {
  for (name in c("test.Rmd", "test.Rnw")){
    file <- file.path("test", "knitr", name) %>%
      system.file(package = "drake", mustWork = TRUE)
    expect_true(file.copy(
      from = file,
      to = getwd(),
      recursive = TRUE,
      overwrite = TRUE
    ))
  }
  ans <- sort(paste0("target", seq_len(18)))
  for (doc in c("'test.Rmd'", "test.Rmd", "test.Rnw"))
    expect_equal(sort(deps_in_document(doc)), ans)
  expect_false(file.exists("test.md"))
  expect_error(dknit("test.Rmd"))
  expect_silent(tmp <- dknit("'test.Rmd'", quiet = TRUE))
  expect_true(file.exists("test.md"))
  expect_warning(x <- deps_in_document("report.Rmd"))
  expect_equal(x, character(0))
  load_basic_example()
  x <- deps_in_document("report.Rmd")
  expect_equal(sort(x), sort(c(
    "small", "coef_regression2_small", "large"
  )))
  expect_equal("targ", find_knitr_targets(function(x){
    readd(targ)
  }))
})

test_with_dir("find_dknit_doc() works", {
  false <- c(
    "dknit",
    "dknit()",
    "knit(4)",
    "other(f)",
    "dknittles(f('file.Rmd'))",
    "other::dknit('file.Rmd')",
    "other:::dknit('file.Rmd')"
  )
  true <- list(
    "dknit('file.Rmd')",
    "drake::dknit('file.Rmd')",
    "drake:::dknit('file.Rmd')",
    function(x){
      dknit("file.Rmd")
    },
    "f(g(dknit('file.Rmd', output = 'file.md', quiet = TRUE)))",
    "f(g(dknit(output = 'file.md', quiet = TRUE, input = 'file.Rmd') + 5))",
    "f(g(dknit(output = 'file.md', quiet = TRUE, 'file.Rmd') + 5))"
  )
  for (cmd in false){
    expect_equal(find_dknit_doc(cmd), character(0))
  }
  for (cmd in true){
    expect_equal(find_dknit_doc(cmd), "file.Rmd")
  }
})

test_with_dir("misc knitr", {
  o <- get_specific_arg(list(a = parse(text = "1 <- 2")), name = "a")
  expect_equal(o, character(0))
  f <- function()
  expect_silent(o <- doc_of_function_call(dknit))
  f <- function(x){
    dknit("file.Rmd")
  }
  doc_of_function_call(f)
})
