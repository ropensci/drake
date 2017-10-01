cat(get_testing_scenario_name(), ": ", sep = "")
context("import package")

test_with_dir("react to change in package", {
  lib <- "local_lib"
  dir.create(lib)
  pkgenv <- new.env()
  pkgenv$newfunction <- function(x){
    x + 1
  }
  withr::with_message_sink(
    new = tempfile(),
    code = {
      package.skeleton(name = "newpkg", environment = pkgenv)
    }
  )
  unlink(file.path("newpkg", "man"), recursive = TRUE)
  install.packages("newpkg", type = "source", repos = NULL,
    lib = lib, quiet = TRUE)

  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism
  load_basic_example(envir = e)
  e$reg2 <- function(d) {
    y <- newpkg:::newfunction(10)
    d$x2 <- d$x ^ 2
    lm(y ~ x2, data = d)
  }
  my_plan <- e$my_plan
  withr::with_libpaths(
    new = c(lib, .libPaths()),
    code = {
      con <- config(my_plan, envir = e,
        jobs = jobs, parallelism = parallelism,
        verbose = FALSE)
      testrun(con)
      expect_equal(sort(justbuilt(con)), sort(my_plan$target))
      expect_equal(sort(outdated(my_plan, envir = e, verbose = FALSE)),
        character(0))
    }
  )

  # Different installation locations should
  # still give the same hash.
  lib2 <- "local_lib2"
  dir.create(lib2)
  install.packages("newpkg", type = "source", repos = NULL,
    lib = lib2, quiet = TRUE)
  withr::with_libpaths(
    new = c(lib, .libPaths()),
    code = {
      unloadNamespace("newpkg")
      hash1 <- rehash_package("package:newpkg", config = con)
    }
  )
  withr::with_libpaths(
    new = c(lib2, .libPaths()),
    code = {
      unloadNamespace("newpkg")
      hash2 <- rehash_package("package:newpkg", config = con)
    }
  )
  expect_equal(hash1, hash2)

  # Reinstall the same package and check that stuff is still up to date.
  for (i in seq_len(2)){
    withr::with_libpaths(
      new = c(lib2, .libPaths()),
      code = {
        unloadNamespace("newpkg")
        expect_equal(sort(outdated(my_plan, envir = e, verbose = FALSE)),
          character(0))
        con <- config(my_plan, envir = e,
          jobs = jobs, parallelism = parallelism,
          verbose = FALSE)
        testrun(con)
        expect_equal(sort(justbuilt(con)), character(0))
        expect_equal(sort(outdated(my_plan, envir = e, verbose = FALSE)),
          character(0))
      }
    )
    install.packages("newpkg", type = "source", repos = NULL,
      lib = lib2, quiet = TRUE)
  }

  # Install an altered copy of the package with the same version number.
  # Targets should be out of date.
  unlink("newpkg", recursive = TRUE)
  pkgenv$newfunction <- function(x){
    x + 2
  }
  withr::with_message_sink(
    new = tempfile(),
    code = {
      package.skeleton(name = "newpkg", environment = pkgenv)
    }
  )
  unlink(file.path("newpkg", "man"), recursive = TRUE)
  install.packages("newpkg", type = "source", repos = NULL,
    lib = lib, quiet = TRUE)
  obj <- sort(c("'report.md'", "report_dependencies",
    my_plan$target[grepl("regression2", my_plan$target)]))
  withr::with_libpaths(
    new = c(lib, .libPaths()),
    code = {
      unloadNamespace("newpkg")
      expect_equal(sort(outdated(my_plan, envir = e, verbose = FALSE)),
        obj)
      con <- config(my_plan, envir = e,
        jobs = jobs, parallelism = parallelism,
        verbose = FALSE)
      testrun(con)
      expect_equal(sort(justbuilt(con)), setdiff(obj,
        c("'report.md'", "report_dependencies")))
      expect_equal(sort(outdated(my_plan, envir = e, verbose = FALSE)),
        character(0))
    }
  )
})
