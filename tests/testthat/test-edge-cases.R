drake_context("edge cases")

test_with_dir("lock_envir works", {
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism
  caching <- scenario$caching
  plan <- drake_plan(
    x = testthat::expect_error(
      assign("a", 1, envir = parent.env(drake_envir())),
      regexp = "binding"
    )
  )
  make(
    plan,
    envir = e,
    jobs = jobs,
    parallelism = parallelism,
    caching = caching,
    lock_envir = TRUE,
    verbose = T,
    session_info = FALSE
  )
  e$a <- 123
  e$plan$four <- "five"
  plan <- drake_plan(
    x = assign("a", 1, envir = parent.env(drake_envir()))
  )
  make(
    plan,
    envir = e,
    jobs = jobs,
    parallelism = parallelism,
    caching = caching,
    lock_envir = FALSE,
    verbose = FALSE,
    session_info = FALSE
  )
  expect_true("x" %in% cached())
})

test_with_dir("Try to modify a locked environment", {
  e <- new.env()
  lock_environment(e)
  plan <- drake_plan(x = {
    e$a <- 1
    2
  })
  expect_error(
    make(plan, session_info = FALSE, cache = storr::storr_environment()),
    regexp = "reproducibility"
  )
})

test_with_dir("skip everything", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  f <- function(x) {
    x
  }
  pl <- drake_plan(a = f(0))
  make(
    pl,
    session_info = FALSE,
    skip_targets = TRUE,
    skip_imports = TRUE
  )
  con <- drake_config(
    pl,
    session_info = FALSE,
    skip_targets = TRUE,
    skip_imports = TRUE
  )
  expect_equal(justbuilt(con), character(0))
})

test_with_dir("can keep going", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  parallelism <- scenario$parallelism
  jobs <- scenario$jobs
  e$fail <- function(...) {
    stop("oops")
  }
  e$succeed <- function(...) {
    invisible()
  }
  plan <- drake_plan(
    a1 = fail(),
    a2 = succeed(),
    a3 = succeed(),
    a4 = fail(),
    b1 = fail(a1),
    b2 = succeed(a2),
    b3 = succeed(a3),
    b4 = succeed(a4)
  )
  # warnings depend on the parallelism
  suppressWarnings(
    make(
      plan,
      keep_going = TRUE,
      parallelism = parallelism,
      verbose = FALSE,
      jobs = jobs,
      envir = e,
      session_info = FALSE
    )
  )
  expect_equal(sort(cached()),
               sort(c("a2", "a3", "b2", "b3", "b4", "fail", "succeed")))
  expect_equal(sort(failed()), sort(c("a1", "a4", "b1")))
})

test_with_dir("failed targets do not become up to date", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  fail <- FALSE
  plan <- drake_plan(
    d = 3,
    a = {
      if (fail) {
        stop("my failure message")
      } else {
        d
      }
    },
    b = 5,
    c = list(a, b)
  )
  make(plan)
  con <- drake_config(plan)
  expect_equal(sort(justbuilt(con)), sort(letters[1:4]))
  fail <- TRUE
  expect_error(make(plan))
  expect_error(make(plan))
  meta <- diagnose(a)
  expect_true(grepl("my failure message", meta$error$message, fixed = TRUE))
  con <- drake_config(plan)
  expect_equal(sort(outdated(con)), sort(c("a", "c")))
})

test_with_dir("config and make without safety checks", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- drake_plan(
    file = readRDS(file_in("my_file.rds"))
  )
  tmp <- drake_config(x, verbose = FALSE)
  expect_silent(
    tmp <- drake_config(x, skip_safety_checks = TRUE, verbose = FALSE))
  expect_silent(config_checks(config = tmp))
})

test_with_dir("Strings stay strings, not symbols", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_silent(x <- drake_plan(a = "A"))
  expect_silent(make(x, verbose = FALSE, session_info = FALSE))
})

test_with_dir("error handlers", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(error_na(1), NA_character_)
  expect_false(error_false(1))
  expect_equal(error_character0(1), character(0))
  expect_null(error_null(1))
  expect_error(error_tibble_times(123))
})

test_with_dir("clean a nonexistent cache", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  clean(list = "no_cache")
  expect_false(file.exists(default_cache_path()))
})

test_with_dir("stringsAsFactors can be TRUE", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  f <- function(x) {
    return(x)
  }
  myplan <- data.frame(target = "a", command = "f(\"helloworld\")",
    stringsAsFactors = TRUE)
  expect_true(is.factor(myplan$target))
  expect_true(is.factor(myplan$command))
  make(myplan, verbose = FALSE, session_info = FALSE)
  expect_equal(readd(a), "helloworld")
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
                 regexp = "Unloading targets from environment")
  config$plan$target <- "repeated"
  expect_error(drake_config(plan = config$plan))
})

test_with_dir("target conflicts with previous import", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
  testrun(config)
  config$plan$command[[2]] <- quote(g(1+1))
  new_row <- drake_plan(f = 1 + 1)
  config$plan <- bind_plans(config$plan, new_row)
  config$targets <- config$plan$target
  testrun(config)
  expect_equal(
    justbuilt(config),
    sort(c("drake_target_1", "combined", "f", "final", "yourinput"))
  )
})

test_with_dir("true targets can be functions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  generator <- function() return(function(x) {
    x + 1
  })
  plan <- drake_plan(myfunction = generator(), output = myfunction(1))
  make(plan, verbose = FALSE, session_info = FALSE)
  config <- drake_config(plan, verbose = FALSE, session_info = FALSE)
  expect_equal(readd(output), 2)
  expect_true(is.function(config$cache$get("myfunction")))
  myfunction <- readd(myfunction)
  expect_equal(myfunction(4), 5)
})

test_with_dir("GitHub issue 460", {
  plan <- drake_plan(a = base::sqrt(1), b = a, c = b)
  config <- drake_config(
    plan,
    targets = "b",
    cache = storr::storr_environment()
  )
  expect_equal(sort(igraph::V(config$schedule)$name), sort(letters[1:2]))
  expect_equal(
    intersect(
      igraph::V(config$schedule)$name,
      igraph::V(config$imports)$name
    ),
    character(0)
  )
  expect_true(
    encode_namespaced("base::sqrt") %in% igraph::V(config$imports)$name)
  process_targets(config)
})

test_with_dir("warning when file_out() files not produced", {
  skip_on_cran()
  plan <- drake_plan(
    x = {
      file.create(file_out("a"))
      file_out("b", "c")
    }
  )
  expect_warning(
    make(plan, cache = storr::storr_environment(), session_info = FALSE),
    regexp = "Missing files for target"
  )
})

test_with_dir("file hash of a non-file", {
  expect_true(is.na(file_hash("asdf", list())))
  expect_true(is.na(rehash_file("asdf", list())))
  expect_true(is.na(file_hash(encode_path("asdf"), list())))
  expect_true(is.na(rehash_file(encode_path("asdf"), list())))
})

test_with_dir("imported functions cannot depend on targets", {
  global_import <- 1
  my_fun <- function(){
    global_import
    other_fun <- function(){
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
  expect_equal(unlist(deps, use.names = FALSE), "global_import")
})

test_with_dir("case sensitivity", {
  plan <- drake_plan(
    a = 1,
    b = 2,
    B = A(),
    c = 15
  )
  A <- function(){
    1 + 1
  }
  expect_warning(
    config <- drake_config(
      plan,
      cache = storr::storr_environment(),
      session_info = FALSE
    ),
    regexp = "case insensitive"
  )
})

test_with_dir("empty deps_graph()", {
  expect_equal(deps_graph(NULL, 1, 2), character(0))
})
