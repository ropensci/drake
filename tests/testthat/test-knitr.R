drake_context("knitr")

test_with_dir("empty cases", {
  expect_equal(knitr_deps_list(NULL), list())
  expect_equal(safe_get_tangled_frags(NULL), character(0))
})

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
    file_store(paste0("file", seq_len(6)))
  ))
  expect_equal(sort(knitr_deps("'test.Rmd'")), ans)
  expect_false(file.exists("test.md"))
  expect_warning(x <- sort(knitr_deps("report.Rmd")))
  expect_warning(expect_equal(x, sort(knitr_deps("\"report.Rmd\""))))
  expect_equal(x, character(0))
  load_mtcars_example()
  x <- knitr_deps("report.Rmd")
  y <- expect_warning(deps_code("knit('report.Rmd')"))
  z <- expect_warning(deps_code("render('report.Rmd')"))
  w <- deps_code("funct(knitr_in(report.Rmd))")
  real_deps <- c(
    "small", "coef_regression2_small", "large"
  )
  expect_equal(sort(x), sort(real_deps))
  expect_equal(sort(y), sort(c(real_deps, "knit", "\"report.Rmd\"")))
  expect_equal(sort(z), sort(c(real_deps, "render", "\"report.Rmd\"")))
  expect_equal(sort(w), sort(c("funct")))
})

test_with_dir("find_knitr_doc() works", {
  false <- c(
    "knit",
    "knit()",
    "dknit(4)",
    "other(f)",
    "knittles(f('file.Rmd'))",
    "other::knit('file.Rmd')",
    "drake:::knit('file.Rmd')",
    "render",
    "render()",
    "rendermania(f('file.Rmd'))",
    "other::render('file.Rmd')",
    "drake:::render('file.Rmd')"
  )
  true <- list(
    "knit('file.Rmd')",
    "knitr::knit(input = 'file.Rmd')",
    "knitr:::knit('file.Rmd')",
    function(x){
      knit("file.Rmd")
    },
    "f(g(knit('file.Rmd', output = 'file.md', quiet = TRUE)))",
    "f(g(knit(output = 'file.md', quiet = TRUE, input = 'file.Rmd') + 5))",
    "f(g(knit(output = 'file.md', quiet = TRUE, 'file.Rmd') + 5))",
    "render(input = 'file.Rmd')",
    "rmarkdown::render('file.Rmd')",
    "rmarkdown:::render('file.Rmd')",
    function(x){
      render("file.Rmd")
    },
    "f(g(render('file.Rmd', output_file = 'file.md', quiet = TRUE)))",
    "f(g(render(output_file = 'file.md', quiet = TRUE, input = 'file.Rmd') + 5))", # nolint
    "f(g(render(output_file = 'file.md', quiet = TRUE, 'file.Rmd') + 5))"
  )
  for (cmd in false){
    expect_equal(find_knitr_doc(cmd), character(0))
  }
  for (cmd in true){
    expect_equal(find_knitr_doc(cmd), "file.Rmd")
  }
})

test_with_dir("edge cases finding knitr docs", {
  expect_equal(find_knitr_doc("knit(a, b)"), "a")
  expect_equal(find_knitr_doc("knit(quiet = TRUE)"), character(0))
  expect_equal(deps_code("knit(quiet = TRUE)"), "knit")
})

test_with_dir("knitr file deps from commands and functions", {
  load_mtcars_example()
  expect_equal(sort(deps_code("'report.Rmd'")), sort(c(
    "coef_regression2_small", "large", "small"
  )))
  f <- function(x){
    knit(x)
  }
  expect_equal(deps_code(f), "knit")
})

test_with_dir("misc knitr", {
  f <- function()
  expect_silent(o <- doc_of_function_call(knit))
  f <- function(x){
    knitr::knit("file.Rmd")
  }
  doc_of_function_call(f)
  expect_equal(doc_of_function_call(as.expression(1)), character(0))
  expect_equal(doc_of_function_call(list(1, 2, 3)), "2")
  expect_equal(find_knitr_doc(NULL), character(0))
})
