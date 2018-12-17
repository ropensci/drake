drake_context("code analysis")

test_with_dir("busy function", {
  f <- function(a = 1, b = k(i), nineteen) {
    for (iter in 1:10) {
      got_for <- got_for + iter
    }
    while (iter2 %in% 1:10) {
      got_while <- got_while + iter2
    }
    assign("iter3", val1)
    delayedAssign(x = "iter4", value = val2)
    x <- g(a + b) + iter + iter2 + iter3 + iter4
    g(a - b) -> y
    z = g(a * b) # nolint
    local({
      xyz1 <- 5
    })
    h <- function() {
      xyz2 <- 6
    }
    abc <- xyz1 + xyz2
    f2 <- "local"
    lm(f1 ~ f2 + f3)
    file_in("x")
    drake::file_out("y")
    base::c(got, basevar)
    quote(quoted)
    Quote(quoted2)
    expression(quoted3)
  }
  out <- analyze_code(f)
  expect_equal(out$file_in, "\"x\"")
  expect_equal(out$file_out, "\"y\"")
  expect_equal(out$namespaced, "base::c")
  exp <- sort(c(
    "assign", "basevar", "delayedAssign", "for",
    "f1", "f3", "g", "got", "got_for", "got_while",
    "i", "iter2", "k",  "lm", "local",
    "val1", "val2", "while", "xyz1", "xyz2"
  ))
  expect_equal(sort(out$globals), exp)
})
