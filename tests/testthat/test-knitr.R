drake_context("knitr")

test_with_dir("unparsable pieces of commands are handled correctly", {
  x <- "bluh$"
  expect_false(is_parsable(x))
  expect_equal(find_knitr_doc(x), character(0))
})

test_with_dir("knitr_deps() works", {
  file <- system.file(
    file.path("testing", "test.Rmd"),
    package = "drake", mustWork = TRUE
  )
  expect_true(file.copy(
    from = file,
    to = getwd(),
    recursive = TRUE,
    overwrite = TRUE
  ))
  ans <- sort(c(
    paste0("target", seq_len(18)),
    as_drake_filename(paste0("file", seq_len(6)))
  ))
  expect_equal(sort(knitr_deps("'test.Rmd'")), ans)
  expect_false(file.exists("test.md"))
  expect_warning(x <- knitr_deps("report.Rmd"))
  expect_equal(x, character(0))
  expect_equal(0, length(knitr_deps(character(0))))
  load_basic_example()
  x <- knitr_deps("report.Rmd")
  expect_equal(sort(x), sort(c(
    "small", "coef_regression2_small", "large"
  )))
  expect_equal("targ", find_knitr_targets(function(x){
    readd(targ)
  }))
})

test_with_dir("find_knitr_doc() works", {
  false <- c(
    "knit",
    "knit()",
    "dknit(4)",
    "other(f)",
    "knittles(f('file.Rmd'))",
    "other::knit('file.Rmd')",
    "drake:::knit('file.Rmd')"
  )
  true <- list(
    "knit('file.Rmd')",
    "knitr::knit('file.Rmd')",
    "knitr:::knit('file.Rmd')",
    function(x){
      knit("file.Rmd")
    },
    "f(g(knit('file.Rmd', output = 'file.md', quiet = TRUE)))",
    "f(g(knit(output = 'file.md', quiet = TRUE, input = 'file.Rmd') + 5))",
    "f(g(knit(output = 'file.md', quiet = TRUE, 'file.Rmd') + 5))"
  )
  for (cmd in false){
    expect_equal(find_knitr_doc(cmd), character(0))
  }
  for (cmd in true){
    expect_equal(find_knitr_doc(cmd), "file.Rmd")
  }
})

test_with_dir("misc knitr", {
  o <- get_specific_arg(list(a = parse(text = "1 <- 2")), name = "a")
  expect_equal(o, character(0))
  f <- function()
  expect_silent(o <- doc_of_function_call(knit))
  f <- function(x){
    knit("file.Rmd")
  }
  doc_of_function_call(f)
  expect_equal(doc_of_function_call(as.expression(1)), character(0))
  expect_equal(doc_of_function_call(list(1, 2, 3)), "2")
  expect_equal(find_knitr_doc(NULL), character(0))
})
