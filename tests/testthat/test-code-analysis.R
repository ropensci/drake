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
    "assign", "basevar", "delayedAssign", "expression", "for",
    "f1", "f3", "g", "got", "got_for", "got_while",
    "i", "iter2", "k",  "lm", "local", "Quote", "quote",
    "val1", "val2", "while", "xyz1", "xyz2"
  ))
  expect_equal(sort(out$globals), exp)
})

# https://github.com/cran/codetools/blob/master/tests/tests.R # nolint
test_with_dir("local variable tests from the codetools package", {
  find_locals <- function(expr){
    if (!is.function(expr) && !is.language(expr)) {
      return(list())
    }
    results <- ht_new()
    locals <- ht_new()
    walk_code(expr, results, locals, NULL)
    ht_list(locals)
  }
  expect_equal(find_locals(quote(x <- 1)), "x")
  expect_equal(find_locals(quote(x <- y <- 1)), c("x", "y"))
  expect_equal(find_locals(quote(local(x <- 1))), character(0))
  expect_equal(find_locals(quote(assign(x, 3))), character(0))
  expect_equal(find_locals(quote(assign("x", 3))), "x")
  expect_equal(find_locals(quote(assign("x", 3, 4))), character(0))
})


test_with_dir("same tests with global variables", {
  code <- quote(x <- 1)
  expect_equal(as.character(analyze_code(code)$globals), character(0))
  code <- quote(x <- y <- 1)
  expect_equal(as.character(analyze_code(code)$globals), character(0))
  code <- quote(local(x <- 1))
  expect_equal(analyze_code(code)$globals, "local")
  code <- quote(assign(x, 3))
  out <- sort(analyze_code(code)$globals)
  expect_equal(out, sort(c("assign", "x")))
  code <- quote({
    assign(x, 3)
    x <- 1
  })
  out <- sort(analyze_code(code)$globals)
  expect_equal(out, sort(c("assign", "x")))
  code <- quote({
    x <- 1
    assign(x, 3)
  })
  expect_equal(analyze_code(code)$globals, "assign")
  code <- quote(assign("x", 3))
  out <- sort(analyze_code(code)$globals)
  expect_equal(out, "assign")
  code <- quote(assign("x", 3, 4))
  out <- sort(analyze_code(code)$globals)
  expect_equal(out, "assign")
})

test_with_dir("solitary codetools globals tests", {
  code <- quote({
    local <- 1
    local(x <- 1)
  })
  out <- as.character(analyze_code(code)$globals)
  expect_equal(out, character(0))
  out <- analyze_code(quote(local(x <- 1, e)))$globals
  expect_equal(sort(out), sort(c("local", "e")))
  f <- function(){
    if (is.R()) {
      x
    } else {
      y
    }
  }
  out <- analyze_code(f)$globals
  expect_equal(sort(out), sort(c("if", "is.R", "x", "y")))
  f <- function() {
    if (FALSE) {
      x
    }
  }
  out <- analyze_code(f)$globals
  expect_equal(sort(out), sort(c("if", "x")))

  f <- function(x) {z <- 1; x + y + z} # nolint
  expect_equal(sort(analyze_code(f)$globals), "y")
  expect_equal(analyze_code(function() Quote(x))$globals, "Quote")
  f <- function (f, x, y) {
    local <- f
    local(x <- y)
    x
  }
  expect_equal(analyze_code(f), list())
  f <- function() {
    x <- 1; y <- 2
  }
  out <- as.character(analyze_code(f)$globals)
  expect_equal(out, character(0))
  f <- function(u = x <- 1) {
    y <- 2
  }
  expect_equal(as.character(analyze_code(f)$globals), character(0))
})

test_with_dir("`my_function<-` edge cases", {
  skip("Not ready for `my_function<-` edge cases yet")
  code <- quote(f(x) <- 1)
  out <- analyze_code(code)$globals
  expect_equal(out, "f<-")
  code <- quote(f(g(h(x, w), y(a), z(u, v), 1) <- 1))
  out <- sort(as.character(analyze_code(code)$globals))
  # Does not quite agree with codetools,
  # but it is close, and it seems more consistent:
  exp <- sort(c("f<-", "g<-", "h<-", "a", "u", "v", "w", "y", "z"))
  expect_equal(out, exp)
})
