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
  utils::install.packages("newpkg", type = "source", repos = NULL,
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
      prework <- paste0(".libPaths('", lib, "')")
      con <- config(my_plan, envir = e,
        jobs = jobs, parallelism = parallelism,
        verbose = FALSE, prework = prework)
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
  utils::install.packages("newpkg", type = "source", repos = NULL,
    lib = lib2, quiet = TRUE)
  withr::with_libpaths(
    new = c(lib, .libPaths()),
    code = {
      unloadNamespace("newpkg")
      hash1 <- package_hash("package:newpkg", config = con)
    }
  )
  withr::with_libpaths(
    new = c(lib2, .libPaths()),
    code = {
      unloadNamespace("newpkg")
      hash2 <- package_hash("package:newpkg", config = con)
    }
  )
  expect_equal(hash1, hash2)

  # Reinstall the same package and check that stuff is still up to date.
  withr::with_libpaths(
    new = c(lib2, .libPaths()),
    code = {
      unloadNamespace("newpkg")
      expect_equal(sort(outdated(my_plan, envir = e, verbose = FALSE)),
        character(0))
      prework <- paste0(".libPaths('", lib2, "')")
      con <- config(my_plan, envir = e,
        jobs = jobs, parallelism = parallelism,
        verbose = FALSE, prework = prework)
      testrun(con)
      expect_equal(sort(justbuilt(con)), character(0))
      expect_equal(sort(outdated(my_plan, envir = e, verbose = FALSE)),
        character(0))
    }
  )

  # Install an altered copy of the package with a different version number.
  # Targets should be out of date.
  path <- file.path("newpkg", "DESCRIPTION")
  lines <- readLines(path)
  index <- grep("^Version:", lines)
  lines[index] <- paste0(lines[index], ".9") # Change the version.
  unlink(path)
  writeLines(text = lines, con = path)
  utils::install.packages("newpkg", type = "source", repos = NULL,
    lib = lib, quiet = TRUE)
  obj <- sort(c("'report.md'", "report_dependencies",
    my_plan$target[grepl("regression2", my_plan$target)]))
  withr::with_libpaths(
    new = c(lib, .libPaths()),
    code = {
      unloadNamespace("newpkg")
      expect_equal(sort(outdated(my_plan, envir = e, verbose = FALSE)),
        obj)
      prework <- paste0(".libPaths('", lib, "')")
      con <- config(my_plan, envir = e,
        jobs = jobs, parallelism = parallelism,
        verbose = FALSE, prework = prework)
      testrun(con)
      jb <- justbuilt(con)
      expect_true(length(jb) < nrow(con$plan))
      expect_true(all(c("regression2_small", "regression2_large") %in% jb))
      expect_equal(sort(outdated(my_plan, envir = e, verbose = FALSE)),
        character(0))
    }
  )
})
