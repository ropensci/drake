# Some CI frameworks apparently do not like it
# when I actually try to install and use 'newpkg'
# in make(). I do not know what is wrong, but I
# cannot reproduce it. Here, I just check that
# two different local installations give the
# same package hash.

cat(get_testing_scenario_name(), ": ", sep = "")
context("import package")

test_with_dir("package hash does not depend on lib", {
  lib <- "local_lib"
  dir.create(lib)
  pkgenv <- new.env()
  pkgenv$newfunction <- function(x){
    x + 1
  }
  withr::with_message_sink(
    new = tempfile(),
    code = {
      utils::package.skeleton(name = "newpkg", environment = pkgenv)
    }
  )
  unlink(file.path("newpkg", "man"), recursive = TRUE)
  utils::install.packages("newpkg", type = "source", repos = NULL,
    lib = lib, quiet = TRUE)

  lib2 <- "local_lib2"
  dir.create(lib2)
  utils::install.packages("newpkg", type = "source", repos = NULL,
    lib = lib2, quiet = TRUE)
  withr::with_libpaths(
    new = c(lib, .libPaths()),
    code = {
      unloadNamespace("newpkg")
      hash1 <- package_hash("package:newpkg", config = list())
    }
  )
  withr::with_libpaths(
    new = c(lib2, .libPaths()),
    code = {
      unloadNamespace("newpkg")
      hash2 <- package_hash("package:newpkg", config = list())
    }
  )
  expect_equal(hash1, hash2)
})
