drake_context("imports")

test_with_dir("responses to imported objects and functions", {
  config <- dbug()
  testrun(config)

  # change imported object
  config$envir$c <- config$envir$c + 1
  testrun(config)
  expect_equal(
    justbuilt(config),
    setdiff(sort(config$plan$target), "myinput")
  )

  # change nested function trivially
  eval(parse(text = "g <- function(y) {

      h(  y)+b # comment
    }"),
    envir = config$envir
  )
  testrun(config)
  nobuild(config)

  # change nested function so that it gives the same answer
  eval(parse(text = "g <- function(y) {
      h(y)+b + 1-1 - 0
    }"),
    envir = config$envir
  )
  testrun(config)
  expect_equal(justbuilt(config), sort(c("nextone", "yourinput")))

  # nested function gives different answer
  eval(parse(text = "g <- function(y) {
      h(y)+b + 16
    }"),
    envir = config$envir
  )
  testrun(config)
  expect_true("final" %in% justbuilt(config))

  # test a deeper nested function
  eval(parse(text = "i <- function(x) {
      2*x + sqrt(13)
    }"),
    envir = config$envir
  )
  testrun(config)
  expect_true("final" %in% justbuilt(config))

  # command depends on imported object k
  config$plan$command[[2]] <- quote(f(1 + 1) + k)
  config$envir$k <- 5
  testrun(config)
  final0 <- readd(final)
  builds <- sort(
    c(
      "drake_target_1",
      "combined",
      "final",
      "yourinput"
    )
  )
  expect_equal(justbuilt(config), builds)

  # nothing to do
  testrun(config)
  nobuild(config)
  expect_true(identical(final0, readd(final)))

  # change k
  config$envir$k <- 10
  testrun(config)
  expect_equal(justbuilt(config), builds)
  expect_false(identical(final0, readd(final)))
})

test_with_dir("add a new import", {
  plan <- drake_plan(a = as.integer(sqrt(4)))
  cache <- storr::storr_environment()
  make(plan, cache = cache, session_info = FALSE)
  config <- drake_config(plan, cache = cache, session_info = FALSE)
  expect_equal(justbuilt(config), "a")
  expect_equal(readd(a, cache = cache), 2L)
  sqrt <- function(x) {
    x + 1L
  }
  make(plan, cache = cache, session_info = FALSE)
  config <- drake_config(plan, cache = cache, session_info = FALSE)
  expect_equal(justbuilt(config), "a")
  expect_equal(readd(a, cache = cache), 5L)
})

test_with_dir("expose_imports() works", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  evalq(
    f <- function(x) {
      g(x)
    },
    envir = envir
  )
  evalq(
    g <- function(x) {
      digest(x)
    },
    envir = envir
  )
  plan <- drake_plan(
    x = f(1),
    y = digest::digest(x)
  ) # double-scoped functions stop the nesting.
  config <- drake_config(plan, envir = envir)
  n_nodes <- length(igraph::V(config$graph)$name)
  expect_true(n_nodes < 10)
  envir <- expose_imports(digest, envir = envir)
  config <- drake_config(plan, envir = envir)
  n_nodes_new <- length(igraph::V(config$graph)$name)
  expect_true(n_nodes_new > n_nodes)
  make(config = config)
  expect_is(readd(x), "character")
})

# Target/import conflicts are unpredictable. A warning should
# be enough.
test_with_dir("target conflicts with current import or another target", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
  config$plan <- rbind(config$plan, data.frame(target = "f",
                                               command = "1+1"))
  expect_message(drake_config(plan = config$plan,
                            envir = config$envir),
                 regexp = "unload")
  config$plan$target <- "repeated"
  expect_error(drake_config(plan = config$plan))
})

test_with_dir("target conflicts with previous import", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
  testrun(config)
  config$plan$command[[2]] <- quote(g(1 + 1))
  new_row <- drake_plan(f = 1 + 1)
  config$plan <- bind_plans(config$plan, new_row)
  config$targets <- config$plan$target
  testrun(config)
  config <- drake_config(config$plan)
  expect_equal(
    justbuilt(config),
    sort(c("drake_target_1", "combined", "f", "final", "yourinput"))
  )
})

test_with_dir("imported functions cannot depend on targets", {
  global_import <- 1
  my_fun <- function() {
    global_import
    other_fun <- function() {
      target_in_plan
    }
  }
  plan <- drake_plan(
    target_in_plan = 1,
    my_fun()()
  )
  config <- drake_config(
    plan,
    cache = storr::storr_environment(),
    session_info = FALSE
  )
  deps <- deps_target("my_fun", config)
  expect_equal(deps$name, "global_import")
})
